-- | GHC Core Extraction Shim
--
-- Uses the GHC API to compile a Haskell source file through desugaring,
-- then emits the resulting Core as OrganIR JSON.
--
-- Based on Frankenstein's GhcBridge/Driver.hs and GhcBridge/CoreTranslate.hs.

module OrganBank.GhcShim
  ( extractOrganIR
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text qualified as T
import GHC
import GHC.Core (CoreBind, CoreExpr, CoreProgram, Bind (..), Expr (..), Alt (..), AltCon (..))
import GHC.Core.DataCon (dataConName)
import GHC.Core.TyCo.Rep (Type (..))
import GHC.Core.TyCon (tyConName)
import GHC.Driver.Session (updOptLevel)
import GHC.Types.Literal (Literal (..), LitNumType (..))
import GHC.Types.Name (getOccString, nameModule_maybe)
import GHC.Types.Name qualified as GN
import GHC.Types.Unique (getKey)
import GHC.Types.Var (Var, varName, varType, isTyVar, binderVar, isInvisibleFunArg, ForAllTyBinder)
import GHC.Unit.Module.ModGuts (mg_binds)
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Process (readProcess)

-- | Extract GHC Core from a Haskell source file and emit OrganIR JSON.
extractOrganIR :: FilePath -> IO (Either Text Text)
extractOrganIR inputPath = do
  libdir <- detectLibDir
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' = updOptLevel 1 dflags  -- O1 for demand analysis
    setSessionDynFlags dflags'
    target <- guessTarget inputPath Nothing Nothing
    setTargets [target]
    _sf <- load LoadAllTargets
    modGraph <- getModuleGraph
    let summaries = mgModSummaries modGraph
    case summaries of
      [] -> pure $ Left "No modules found"
      (ms:_) -> do
        parsed    <- parseModule ms
        typecked  <- typecheckModule parsed
        desugared <- desugarModule typecked
        let modGuts  = dm_core_module desugared
            coreProg = mg_binds modGuts
            modName  = moduleNameString (moduleName (ms_mod ms))
        pure $ Right $ emitOrganIR modName inputPath coreProg

detectLibDir :: IO FilePath
detectLibDir = do
  raw <- readProcess "ghc" ["--print-libdir"] ""
  pure $ reverse $ dropWhile (== '\n') $ reverse raw

-- | Emit a CoreProgram as OrganIR JSON using the organ-ir library.
emitOrganIR :: String -> FilePath -> CoreProgram -> Text
emitOrganIR modName srcFile binds =
    renderOrganIR $
        IR.simpleOrganIR IR.LHaskell "ghc-shim-0.1" (T.pack modName) srcFile $
            concatMap bindToDefs binds

-- | Convert a CoreBind into OrganIR definitions.
bindToDefs :: CoreBind -> [IR.Definition]
bindToDefs (NonRec v e) = [mkDef v e]
bindToDefs (Rec pairs)  = [mkDef v e | (v, e) <- pairs]

-- | Build a Definition from a binder and its Core expression.
mkDef :: Var -> CoreExpr -> IR.Definition
mkDef v expr =
    let vname    = varName v
        nameT    = T.pack (getOccString vname)
        uniq     = getKey (GN.nameUnique vname)
        irName   = IR.Name nameT (fromIntegral uniq)
        modPfx   = case nameModule_maybe vname of
                     Just m  -> T.pack (moduleNameString (moduleName m))
                     Nothing -> ""
        irExpr   = translateExpr expr
        (params, _body) = collectLamBinders expr
        arity    = length (filter (not . isTyVar) params)
        irType   = translateType (varType v)
        sort     = if arity > 0 then IR.SFun else IR.SVal
    in IR.Definition
        { IR.defName       = IR.QName modPfx irName
        , IR.defType       = irType
        , IR.defExpr       = irExpr
        , IR.defSort       = sort
        , IR.defVisibility = IR.Public
        , IR.defArity      = arity
        }

-- | Collect lambda binders from nested Lam nodes.
collectLamBinders :: CoreExpr -> ([Var], CoreExpr)
collectLamBinders (Lam b e) =
    let (bs, body) = collectLamBinders e
    in (b : bs, body)
collectLamBinders (Cast e _) = collectLamBinders e
collectLamBinders (Tick _ e) = collectLamBinders e
collectLamBinders e = ([], e)

-------------------------------------------------------------------------------
-- Expression translation
-------------------------------------------------------------------------------

-- | Translate a GHC Core expression to OrganIR.
translateExpr :: CoreExpr -> IR.Expr
translateExpr (Var v) =
    let n = varName v
        nameT = T.pack (getOccString n)
        uniq  = getKey (GN.nameUnique n)
    in IR.EVar (IR.Name nameT (fromIntegral uniq))

translateExpr (Lit lit) = IR.ELit (translateLit lit)

translateExpr e@(App _ _) =
    let (f, args) = collectArgs e
        f'    = translateExpr f
        -- Separate type arguments from value arguments
        (tyArgs, valArgs) = partitionArgs args
        withTyApp = case tyArgs of
                      [] -> f'
                      _  -> IR.ETypeApp f' (map translateTypeArg tyArgs)
    in case valArgs of
         [] -> withTyApp
         _  -> IR.EApp withTyApp (map translateExpr valArgs)

translateExpr e@(Lam _ _) =
    let (binders, body) = collectLamBinders e
        -- Separate type lambda binders from value lambda binders
        (tyBinders, valBinders) = span isTyVar binders
        bodyExpr = translateExpr body
        withLam = case valBinders of
                    [] -> bodyExpr
                    _  -> IR.ELam (map mkLamParam valBinders) bodyExpr
    in case tyBinders of
         [] -> withLam
         _  -> IR.ETypeLam (map mkTyVar tyBinders) withLam

translateExpr (Let (NonRec v e) body) =
    IR.ELet [mkLetBind v e] (translateExpr body)

translateExpr (Let (Rec pairs) body) =
    IR.ELet (map (uncurry mkLetBind) pairs) (translateExpr body)

translateExpr (Case scrut _bndr _ty alts) =
    IR.ECase (translateExpr scrut) (map translateAlt alts)

translateExpr (Cast e _) = translateExpr e

translateExpr (Tick _ e) = translateExpr e

translateExpr (Type _) = IR.EVar (IR.Name "type" 0)

translateExpr (Coercion _) = IR.EVar (IR.Name "coercion" 0)

-- | Collect arguments from nested App nodes.
collectArgs :: CoreExpr -> (CoreExpr, [CoreExpr])
collectArgs = go []
  where
    go acc (App f a) = go (a : acc) f
    go acc e         = (e, acc)

-- | Partition arguments into type args and value args.
-- Type args come from (Type _) nodes in Core.
partitionArgs :: [CoreExpr] -> ([CoreExpr], [CoreExpr])
partitionArgs = foldr classify ([], [])
  where
    classify e@(Type _) (tys, vals) = (e : tys, vals)
    classify e          (tys, vals) = (tys, e : vals)

-- | Extract a type from a Type argument expression.
translateTypeArg :: CoreExpr -> IR.Ty
translateTypeArg (Type t) = translateType t
translateTypeArg _        = IR.TAny

-- | Make a lambda parameter from a GHC Var.
mkLamParam :: Var -> IR.LamParam
mkLamParam v =
    let n = varName v
        nameT = T.pack (getOccString n)
        uniq  = getKey (GN.nameUnique n)
    in IR.LamParam (IR.Name nameT (fromIntegral uniq)) (Just (translateType (varType v)))

-- | Make a type variable from a GHC Var.
mkTyVar :: Var -> IR.TyVar
mkTyVar v =
    let n = varName v
        nameT = T.pack (getOccString n)
        uniq  = getKey (GN.nameUnique n)
    in IR.TyVar (IR.Name nameT (fromIntegral uniq)) Nothing

-- | Make a let binding.
mkLetBind :: Var -> CoreExpr -> IR.LetBind
mkLetBind v e =
    let n = varName v
        nameT = T.pack (getOccString n)
        uniq  = getKey (GN.nameUnique n)
    in IR.LetBind (IR.Name nameT (fromIntegral uniq)) (Just (translateType (varType v))) (translateExpr e)

-------------------------------------------------------------------------------
-- Alternative / branch translation
-------------------------------------------------------------------------------

-- | Translate a GHC Core case alternative to an OrganIR Branch.
translateAlt :: Alt Var -> IR.Branch
translateAlt (Alt (DataAlt dc) binders rhs) =
    let dcN   = dataConName dc
        nameT = T.pack (getOccString dcN)
        uniq  = getKey (GN.nameUnique dcN)
        modPfx = case nameModule_maybe dcN of
                   Just m  -> T.pack (moduleNameString (moduleName m))
                   Nothing -> ""
        qn    = IR.QName modPfx (IR.Name nameT (fromIntegral uniq))
        patBinders = map mkPatBinder binders
    in IR.Branch (IR.PatCon qn patBinders) (translateExpr rhs)

translateAlt (Alt (LitAlt lit) _binders rhs) =
    IR.Branch (IR.PatLit (translateLit lit)) (translateExpr rhs)

translateAlt (Alt DEFAULT _binders rhs) =
    IR.Branch IR.PatWild (translateExpr rhs)

-- | Make a pattern binder from a GHC Var.
mkPatBinder :: Var -> IR.PatBinder
mkPatBinder v =
    let n = varName v
        nameT = T.pack (getOccString n)
        uniq  = getKey (GN.nameUnique n)
    in IR.PatBinder (IR.Name nameT (fromIntegral uniq)) (Just (translateType (varType v)))

-------------------------------------------------------------------------------
-- Literal translation
-------------------------------------------------------------------------------

-- | Translate a GHC Literal to OrganIR Lit.
translateLit :: Literal -> IR.Lit
translateLit (LitChar c)           = IR.LitString (T.singleton c)
translateLit (LitString bs)        = IR.LitString (T.pack (BS8.unpack bs))
translateLit (LitNumber LitNumInt n)    = IR.LitInt n
translateLit (LitNumber LitNumInt8 n)   = IR.LitInt n
translateLit (LitNumber LitNumInt16 n)  = IR.LitInt n
translateLit (LitNumber LitNumInt32 n)  = IR.LitInt n
translateLit (LitNumber LitNumInt64 n)  = IR.LitInt n
translateLit (LitNumber LitNumWord n)   = IR.LitInt n
translateLit (LitNumber LitNumWord8 n)  = IR.LitInt n
translateLit (LitNumber LitNumWord16 n) = IR.LitInt n
translateLit (LitNumber LitNumWord32 n) = IR.LitInt n
translateLit (LitNumber LitNumWord64 n) = IR.LitInt n
translateLit (LitNumber LitNumBigNat n) = IR.LitInt n
translateLit (LitFloat r)          = IR.LitFloat (fromRational r)
translateLit (LitDouble r)         = IR.LitFloat (fromRational r)
translateLit LitNullAddr           = IR.LitString "nulladdr"
translateLit (LitLabel _ _)        = IR.LitString "label"
translateLit (LitRubbish _ _)      = IR.LitString "rubbish"

-------------------------------------------------------------------------------
-- Type translation
-------------------------------------------------------------------------------

-- | Translate a GHC Type to OrganIR Ty.
translateType :: Type -> IR.Ty
translateType (FunTy flag _mult arg res)
    -- Dictionary (constraint) arguments: skip them in the function signature
    -- FTF_T_C = argument is Type, result is Constraint (dict arg in Core)
    -- FTF_C_T = argument is Constraint, result is Type
    -- FTF_C_C = both Constraint
    | isInvisibleFunArg flag = translateType res
    | otherwise =
        case translateType res of
          -- Accumulate function arguments into a single TFn
          IR.TFn args eff retTy ->
              IR.TFn (IR.FnArg Nothing (translateType arg) : args) eff retTy
          resTy ->
              IR.TFn [IR.FnArg Nothing (translateType arg)] (IR.EffectRow [] Nothing) resTy

translateType (TyConApp tc []) =
    let n = tyConName tc
        nameT = T.pack (getOccString n)
        uniq  = getKey (GN.nameUnique n)
        modPfx = case nameModule_maybe n of
                   Just m  -> T.pack (moduleNameString (moduleName m))
                   Nothing -> ""
    in IR.TCon (IR.QName modPfx (IR.Name nameT (fromIntegral uniq)))

translateType (TyConApp tc args) =
    let n = tyConName tc
        nameT = T.pack (getOccString n)
        uniq  = getKey (GN.nameUnique n)
        modPfx = case nameModule_maybe n of
                   Just m  -> T.pack (moduleNameString (moduleName m))
                   Nothing -> ""
    in IR.TApp (IR.QName modPfx (IR.Name nameT (fromIntegral uniq))) (map translateType args)

translateType (TyVarTy v) =
    let n = varName v
        nameT = T.pack (getOccString n)
        uniq  = getKey (GN.nameUnique n)
    in IR.TVar (IR.Name nameT (fromIntegral uniq))

translateType (ForAllTy bndr ty) =
    let (bndrs, innerTy) = collectForAlls ty
        allBndrs = bndr : bndrs
        tvs = map forAllBndrToTyVar allBndrs
    in IR.TForall tvs (translateType innerTy)

translateType (AppTy t1 t2) =
    -- Collect nested AppTy into a single TApp when the head is a known type
    case collectAppTy t1 [translateType t2] of
      (IR.TCon qn, args) -> IR.TApp qn args
      (IR.TApp qn existingArgs, args) -> IR.TApp qn (existingArgs ++ args)
      _ -> IR.TAny  -- truly unknown head (e.g. type variable application)

translateType (LitTy _)      = IR.TAny
translateType (CastTy t _)   = translateType t
translateType (CoercionTy _) = IR.TAny

-- | Collect nested ForAllTy binders.
collectForAlls :: Type -> ([ForAllTyBinder], Type)
collectForAlls (ForAllTy b t) = let (bs, inner) = collectForAlls t in (b : bs, inner)
collectForAlls t = ([], t)

-- | Convert a ForAllTyBinder to an OrganIR TyVar.
forAllBndrToTyVar :: ForAllTyBinder -> IR.TyVar
forAllBndrToTyVar bndr =
    let tv = binderVar bndr
        n  = varName tv
        nameT = T.pack (getOccString n)
        uniq  = getKey (GN.nameUnique n)
    in IR.TyVar (IR.Name nameT (fromIntegral uniq)) Nothing

-- | Collect nested AppTy into the translated head and accumulated args.
collectAppTy :: Type -> [IR.Ty] -> (IR.Ty, [IR.Ty])
collectAppTy (AppTy t1 t2) acc = collectAppTy t1 (translateType t2 : acc)
collectAppTy t acc = (translateType t, acc)
