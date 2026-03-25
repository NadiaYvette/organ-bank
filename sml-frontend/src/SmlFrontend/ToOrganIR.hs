-- | Translate SML AST + inferred types to OrganIR using the organ-ir library.
module SmlFrontend.ToOrganIR (emitOrganIR) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import SmlFrontend.Elab.Env
import SmlFrontend.Elab.Infer (InferState (..))
import SmlFrontend.Elab.Types
import SmlFrontend.Syntax.AST
import SmlFrontend.Syntax.Const
import SmlFrontend.Syntax.Ident

-- | Emit OrganIR JSON for a program with inferred types.
emitOrganIR :: String -> FilePath -> Program -> InferState -> Text
emitOrganIR modName srcFile (Program decs) st =
    let defs = concatMap (decToDefs st) decs
        exports = map (IR.nameText . IR.qnName . IR.defName) defs
    in  renderOrganIR $
            IR.OrganIR
                { IR.irMetadata =
                    IR.Metadata
                        { IR.metaSourceLang = IR.LSml
                        , IR.metaCompilerVersion = Nothing
                        , IR.metaSourceFile = Just (T.pack srcFile)
                        , IR.metaShimVersion = "sml-frontend-0.1"
                        , IR.metaTimestamp = Nothing
                        }
                , IR.irModule =
                    IR.Module
                        { IR.modName = T.pack modName
                        , IR.modExports = exports
                        , IR.modImports = []
                        , IR.modDefs = defs
                        , IR.modDataTypes = []
                        , IR.modEffectDecls = []
                        }
                }

decToDefs :: InferState -> Dec -> [IR.Definition]
decToDefs st = \case
    DVal _ binds -> concatMap (valBindToDef st) binds
    DFun _ binds -> concatMap (funBindToDef st) binds
    DDatatype datbinds -> concatMap datbindToDef datbinds
    DLocal _ decs -> concatMap (decToDefs st) decs
    DSeq decs -> concatMap (decToDefs st) decs
    DPos _ d -> decToDefs st d
    _ -> []

valBindToDef :: InferState -> ValBind -> [IR.Definition]
valBindToDef st (ValBind _ pat exp_) =
    case smlPatName pat of
        Just n ->
            let ty = lookupSmlType n st
             in [IR.funDef (T.pack n) ty (expToIR exp_) (expArity exp_)]
        Nothing -> []

funBindToDef :: InferState -> FunBind -> [IR.Definition]
funBindToDef st (FunBind clauses@(FunClause (VId fname) pats _ _ :| _)) =
    let ty = lookupSmlType (T.unpack fname) st
        arity = length pats
        body = case clauses of
            FunClause _ ps _ b :| [] ->
                let paramNames = concatMap smlPatNames ps
                 in IR.ELam (map (\p -> IR.LamParam (IR.name p) Nothing) paramNames) (expToIR b)
            _ ->
                IR.ELam [IR.LamParam (IR.name "_arg") Nothing] $
                    IR.ECase (IR.EVar (IR.name "_arg")) $
                        map clauseToIR (NE.toList clauses)
     in [IR.funDef fname ty body arity]

clauseToIR :: FunClause -> IR.Branch
clauseToIR (FunClause _ pats _ body) =
    let binders = concatMap smlPatNames pats
     in IR.Branch (IR.PatCon (IR.localName "_") (map (\b -> IR.PatBinder (IR.name b) Nothing) binders)) (expToIR body)

lookupSmlType :: String -> InferState -> IR.Ty
lookupSmlType n st =
    case lookupEnv n (isEnv st) of
        Just (Scheme _ ty) -> ityToTy (apply (isSubst st) ty)
        Nothing -> IR.TAny

ityToTy :: ITy -> IR.Ty
ityToTy = \case
    ITyVar _ -> IR.TAny
    ITyCon c [] -> IR.tCon (T.pack c)
    ITyCon c args -> IR.TApp (IR.localName (T.pack c)) (map ityToTy args)
    ITyFun a b -> IR.tFn [ityToTy a] (ityToTy b)
    ITyTuple ts -> IR.TApp (IR.localName "tuple") (map ityToTy ts)
    ITyRecord _ -> IR.tCon "record"

expToIR :: Exp -> IR.Expr
expToIR = \case
    ESCon sc
        | SInt n <- sc -> IR.ELit (IR.LitInt (fromIntegral n))
        | SReal d <- sc -> IR.ELit (IR.LitFloat d)
        | SString s <- sc -> IR.ELit (IR.LitString s)
        | SChar c <- sc -> IR.ELit (IR.LitString (T.singleton c))
        | otherwise -> IR.EUnreachable
    EVar (LongVId _ (VId n)) -> IR.EVar (IR.name n)
    EApp f arg -> IR.EApp (expToIR f) [expToIR arg]
    EInfix l (VId op) r -> IR.EApp (IR.EVar (IR.name op)) [IR.ETuple [expToIR l, expToIR r]]
    EFn (MRule pat body :| []) ->
        IR.ELam (map (\n -> IR.LamParam (IR.name n) Nothing) (smlPatNames pat)) (expToIR body)
    EFn _ -> IR.ELam [] IR.EUnreachable
    ELet decs body ->
        IR.ELet (zipWith (\i _ -> IR.LetBind (IR.name (T.pack ("_dec" ++ show i))) Nothing IR.EUnreachable) [(0 :: Int) ..] decs) (expToIR body)
    EIf _c t e ->
        IR.ECase
            (IR.ELit (IR.LitBool True))
            [ IR.Branch (IR.PatCon (IR.localName "true") []) (expToIR t)
            , IR.Branch (IR.PatCon (IR.localName "false") []) (expToIR e)
            ]
    ECase scrut rules ->
        IR.ECase (expToIR scrut) (map mruleToIR (NE.toList rules))
    ETuple es -> IR.ETuple (map expToIR es)
    EList es -> IR.EList (map expToIR es)
    ERaise e -> IR.ERaise (expToIR e)
    ESeq [] -> IR.ETuple []
    ESeq (e : es) -> expToIR (NE.last (e :| es))
    EAndalso a b -> expToIR (EIf a b (EVar (LongVId [] (VId "false"))))
    EOrelse a b -> expToIR (EIf a (EVar (LongVId [] (VId "true"))) b)
    ETyped e _ -> expToIR e
    EPos _ e -> expToIR e
    _ -> IR.EUnreachable

mruleToIR :: MRule -> IR.Branch
mruleToIR (MRule pat body) =
    let binders = smlPatNames pat
     in IR.Branch (IR.PatCon (IR.localName "_") (map (\n -> IR.PatBinder (IR.name n) Nothing) binders)) (expToIR body)

-- | Extract the name from a simple pattern.
smlPatName :: Pat -> Maybe String
smlPatName = \case
    PVar (VId n) -> Just (T.unpack n)
    PPos _ p -> smlPatName p
    PTyped p _ -> smlPatName p
    _ -> Nothing

smlPatNames :: Pat -> [Text]
smlPatNames = \case
    PVar (VId n) -> [n]
    PTuple ps -> concatMap smlPatNames ps
    PAs (VId n) p -> n : smlPatNames p
    PPos _ p -> smlPatNames p
    PTyped p _ -> smlPatNames p
    _ -> ["_"]

expArity :: Exp -> Int
expArity = \case EFn _ -> 1; _ -> 0

datbindToDef :: DatBind -> [IR.Definition]
datbindToDef (DatBind _ (TyCon tcName) conbinds) =
    map (conbindToDef tcName) (NE.toList conbinds)

conbindToDef :: Text -> ConBind -> IR.Definition
conbindToDef tcName (ConBind (VId cname) argTy) =
    let arity = case argTy of Nothing -> 0; Just _ -> 1
     in IR.conDef cname (IR.tCon tcName) arity
