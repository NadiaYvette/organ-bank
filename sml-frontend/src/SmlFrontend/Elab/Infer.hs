{- | Hindley-Milner type inference for SML core language.
Implements Algorithm W with value restriction.

Substitutions are threaded monadically à la Mark P. Jones,
"Typing Haskell in Haskell" (1999) — the current substitution
lives in 'InferM' and is applied on demand via 'zonk'.
-}
module SmlFrontend.Elab.Infer (
    InferState (..),
    initInferState,
    infer,
    inferDec,
    inferProgram,
)
where

import Control.Monad ((>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.List (nub, (\\))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import SmlFrontend.Elab.Env
import SmlFrontend.Elab.Types
import SmlFrontend.Syntax.AST
import SmlFrontend.Syntax.Const
import SmlFrontend.Syntax.Ident

-- | Inference state: fresh variable supply + current substitution.
data InferState = InferState
    { isSupply :: !TyUID
    , isSubst :: !Subst
    , isEnv :: !TyEnv
    , isConEnv :: !ConEnv
    }
    deriving (Show)

initInferState :: TyEnv -> ConEnv -> InferState
initInferState = InferState 0 emptySubst

type InferM a = ExceptT String (State InferState) a

runInfer :: InferState -> InferM a -> Either String (a, InferState)
runInfer st m = case runState (runExceptT m) st of
    (Left err, _) -> Left err
    (Right a, st') -> Right (a, st')

------------------------------------------------------------------------
-- Monadic primitives (Jones-style)
------------------------------------------------------------------------

-- | Generate a fresh unification variable.
fresh :: InferM ITy
fresh = do
    st <- lift get
    let u = isSupply st
    lift $ put st{isSupply = u + 1}
    return (ITyVar u)

{- | Apply the current substitution to a type, chasing all bindings.
This is the central operation of the monadic substitution approach:
instead of threading 'Subst' values explicitly, callers simply
'zonk' a type when they need its most-resolved form.
-}
zonk :: ITy -> InferM ITy
zonk ty = do
    s <- isSubst <$> lift get
    return (apply s ty)

-- | Extend the substitution: bind a unification variable to a type.
addSubst :: TyUID -> ITy -> InferM ()
addSubst u t = do
    st <- lift get
    lift $ put st{isSubst = Map.insert u t (isSubst st)}

------------------------------------------------------------------------
-- Monadic environment operations
------------------------------------------------------------------------

-- | Read the current type environment.
getEnv :: InferM TyEnv
getEnv = isEnv <$> lift get

-- | Read the current constructor environment.
getConEnv :: InferM ConEnv
getConEnv = isConEnv <$> lift get

-- | Modify the type environment.
modifyEnv :: (TyEnv -> TyEnv) -> InferM ()
modifyEnv f = do
    st <- lift get
    lift $ put st{isEnv = f (isEnv st)}

-- | Modify the constructor environment.
modifyConEnv :: (ConEnv -> ConEnv) -> InferM ()
modifyConEnv f = do
    st <- lift get
    lift $ put st{isConEnv = f (isConEnv st)}

{- | Run an action with extra bindings in scope, then restore the
original environment. This captures the save/restore pattern that
previously required manually stashing and replacing 'isEnv'.
-}
withBindings :: [(String, Scheme)] -> InferM a -> InferM a
withBindings bindings action = do
    oldEnv <- getEnv
    modifyEnv (extendEnvMany bindings)
    result <- action
    modifyEnv (const oldEnv)
    return result

------------------------------------------------------------------------
-- Unification
------------------------------------------------------------------------

-- | Unify two types under the current substitution.
unify :: ITy -> ITy -> InferM ()
unify t1 t2 = do
    t1' <- zonk t1
    t2' <- zonk t2
    unify' t1' t2'

unify' :: ITy -> ITy -> InferM ()
unify' (ITyVar u) t = bindVar u t
unify' t (ITyVar u) = bindVar u t
unify' (ITyCon c1 as1) (ITyCon c2 as2)
    | c1 == c2 && length as1 == length as2 = mapM_ (uncurry unify) (zip as1 as2)
    | otherwise = throwE $ "Cannot unify " ++ c1 ++ " with " ++ c2
unify' (ITyFun a1 r1) (ITyFun a2 r2) = unify a1 a2 >> unify r1 r2
unify' (ITyTuple ts1) (ITyTuple ts2)
    | length ts1 == length ts2 = mapM_ (uncurry unify) (zip ts1 ts2)
    | otherwise = throwE "Tuple type mismatch"
unify' (ITyRecord fs1) (ITyRecord fs2)
    | map fst fs1 == map fst fs2 = mapM_ (uncurry unify) (zip (map snd fs1) (map snd fs2))
    | otherwise = throwE "Record type mismatch"
unify' t1 t2 = throwE $ "Cannot unify " ++ show t1 ++ " with " ++ show t2

bindVar :: TyUID -> ITy -> InferM ()
bindVar u t
    | t == ITyVar u = return () -- Trivial
    | u `elem` ftv t = throwE $ "Occurs check: " ++ show u ++ " in " ++ show t
    | otherwise = addSubst u t

------------------------------------------------------------------------
-- Instantiation & Generalization
------------------------------------------------------------------------

-- | Instantiate a scheme with fresh variables.
instantiate :: Scheme -> InferM ITy
instantiate (Scheme [] t) = return t
instantiate (Scheme vs t) = do
    freshVars <- mapM (const fresh) vs
    let mapping = Map.fromList (zip vs freshVars)
    return (applyMapping mapping t)
  where
    applyMapping m (ITyVar u) = case Map.lookup u m of Just t' -> t'; Nothing -> ITyVar u
    applyMapping m (ITyCon c as) = ITyCon c (map (applyMapping m) as)
    applyMapping m (ITyFun a b) = ITyFun (applyMapping m a) (applyMapping m b)
    applyMapping m (ITyTuple ts) = ITyTuple (map (applyMapping m) ts)
    applyMapping m (ITyRecord fs) = ITyRecord [(l, applyMapping m t') | (l, t') <- fs]

{- | Generalize a type over variables not free in the environment.
Implements the value restriction: only generalize if the binding is
syntactically a value (variable, fn, constructor).
-}
generalize :: ITy -> Bool -> InferM Scheme
generalize t isValue = do
    t' <- zonk t
    env <- getEnv
    s <- isSubst <$> lift get
    let envFree = ftvEnv (applySubstEnv s env)
        tyFree = nub (ftv t')
        genVars = if isValue then tyFree \\ envFree else []
    return (Scheme genVars t')

-- | Is an expression syntactically a value? (Value restriction)
isSyntacticValue :: Exp -> Bool
isSyntacticValue (EVar _) = True
isSyntacticValue (ESCon _) = True
isSyntacticValue (EFn _) = True
isSyntacticValue (ETuple es) = all isSyntacticValue es
isSyntacticValue (EList es) = all isSyntacticValue es
isSyntacticValue (ERecord fs) = all (isSyntacticValue . snd) fs
isSyntacticValue (ETyped e _) = isSyntacticValue e
isSyntacticValue (EPos _ e) = isSyntacticValue e
isSyntacticValue _ = False

------------------------------------------------------------------------
-- Variable lookup
------------------------------------------------------------------------

-- | Look up a variable in the environment.
lookupVar :: String -> InferM Scheme
lookupVar x = do
    env <- getEnv
    case lookupEnv x env of
        Just s -> return s
        Nothing -> do
            cenv <- getConEnv
            case lookupCon x cenv of
                Just (s, _) -> return s
                Nothing -> throwE $ "Unbound variable: " ++ x

------------------------------------------------------------------------
-- Expression inference
------------------------------------------------------------------------

-- | Infer the type of an expression.
infer :: Exp -> InferM ITy
-- Literals
infer (ESCon (SInt _)) = return tyInt
infer (ESCon (SReal _)) = return tyReal
infer (ESCon (SString _)) = return tyString
infer (ESCon (SChar _)) = return tyChar
-- Variables
infer (EVar (LongVId strs (VId name))) = do
    let fullName = case strs of
            [] -> T.unpack name
            _ -> T.unpack (T.intercalate "." (map (\(StrId s) -> s) strs ++ [name]))
    -- Try full qualified name first, then short name
    s <- case strs of
        [] -> lookupVar fullName
        _ -> do
            env <- getEnv
            case lookupEnv fullName env of
                Just sch -> return sch
                Nothing -> lookupVar (T.unpack name)
    instantiate s
-- Function application
infer (EApp f arg) = do
    tf <- infer f
    ta <- infer arg
    tr <- fresh
    unify tf (ITyFun ta tr)
    return tr
-- Lambda: fn match (single rule)
infer (EFn (MRule pat body :| [])) = do
    (bindings, tpat) <- inferPat pat
    tbody <- withBindings bindings (infer body)
    return (ITyFun tpat tbody)
-- Lambda: fn match (multiple rules)
infer (EFn rules) = do
    targ <- fresh
    tres <- fresh
    mapM_ (inferRule targ tres) rules
    return (ITyFun targ tres)
-- Let expression
infer (ELet decs body) = do
    mapM_ inferDec decs
    infer body
-- If-then-else
infer (EIf cond thn els) = do
    tc <- infer cond
    unify tc tyBool
    tt <- infer thn
    te <- infer els
    unify tt te
    return tt
-- Case expression
infer (ECase scrut rules) = do
    ts <- infer scrut
    tr <- fresh
    mapM_
        ( \(MRule pat body) -> do
            (bindings, tp) <- inferPat pat
            unify ts tp
            tb <- withBindings bindings (infer body)
            unify tr tb
        )
        rules
    return tr
-- Tuple
infer (ETuple es) = ITyTuple <$> mapM infer es
-- List
infer (EList []) = tyList <$> fresh
infer (EList (e : es)) = do
    t <- infer e
    mapM_ (infer >=> unify t) es
    return (tyList t)
-- Record
infer (ERecord fields) = do
    tys <- mapM (\(Lab l, e) -> do t <- infer e; return (T.unpack l, t)) fields
    return (ITyRecord tys)
-- Type annotation
infer (ETyped e _ty) = infer e -- TODO: check annotation
-- Infix
infer (EInfix l (VId op) r) = infer (EApp (EVar (LongVId [] (VId op))) (ETuple [l, r]))
-- Sequencing
infer (ESeq []) = return tyUnit
infer (ESeq [e]) = infer e
infer (ESeq (e : es)) = infer e >> infer (ESeq es)
-- Andalso / Orelse
infer (EAndalso a b) = do
    ta <- infer a
    unify ta tyBool
    tb <- infer b
    unify tb tyBool
    return tyBool
infer (EOrelse a b) = do
    ta <- infer a
    unify ta tyBool
    tb <- infer b
    unify tb tyBool
    return tyBool
-- Raise
infer (ERaise e) = do
    te <- infer e
    unify te tyExn
    fresh -- raise can have any type
    -- Handle
infer (EHandle e rules) = do
    te <- infer e
    mapM_ (inferRule tyExn te) rules
    return te
-- Selector
infer (ESelector _) = do a <- fresh; ITyFun a <$> fresh -- simplified
-- While
infer (EWhile cond body) = do
    tc <- infer cond
    unify tc tyBool
    _ <- infer body
    return tyUnit
-- Position wrapper
infer (EPos _ e) = infer e
infer e = throwE $ "Cannot infer type of: " ++ show e

------------------------------------------------------------------------
-- Match rules
------------------------------------------------------------------------

-- | Infer a match rule, unifying with expected scrutinee and result types.
inferRule :: ITy -> ITy -> MRule -> InferM ()
inferRule scrutTy resTy (MRule pat body) = do
    (bindings, tp) <- inferPat pat
    unify scrutTy tp
    tb <- withBindings bindings (infer body)
    unify resTy tb

------------------------------------------------------------------------
-- Pattern inference
------------------------------------------------------------------------

-- | Infer the type of a pattern, returning bindings and the pattern type.
inferPat :: Pat -> InferM ([(String, Scheme)], ITy)
inferPat PWild = do a <- fresh; return ([], a)
inferPat (PVar (VId name)) = do
    a <- fresh
    return ([(T.unpack name, monoScheme a)], a)
inferPat (PSCon (SInt _)) = return ([], tyInt)
inferPat (PSCon (SReal _)) = return ([], tyReal)
inferPat (PSCon (SString _)) = return ([], tyString)
inferPat (PSCon (SChar _)) = return ([], tyChar)
inferPat (PSCon (SWord _)) = return ([], tyInt)
inferPat (PTuple pats) = do
    results <- mapM inferPat pats
    let bindings = concatMap fst results
        types = map snd results
    return (bindings, ITyTuple types)
inferPat (PList []) = do a <- fresh; return ([], tyList a)
inferPat (PList (p : ps)) = do
    (bs1, t) <- inferPat p
    bss <- mapM (\p' -> do (bs, t') <- inferPat p'; unify t t'; return bs) ps
    return (bs1 ++ concat bss, tyList t)
inferPat (PCon (LongVId _ (VId name)) Nothing) = do
    cenv <- getConEnv
    case lookupCon (T.unpack name) cenv of
        Just (s, _) -> do t <- instantiate s; return ([], t)
        Nothing -> do a <- fresh; return ([(T.unpack name, monoScheme a)], a) -- treat as variable
inferPat (PCon (LongVId _ (VId name)) (Just argPat)) = do
    cenv <- getConEnv
    case lookupCon (T.unpack name) cenv of
        Just (s, _) -> do
            tc <- instantiate s
            (argBinds, argTy) <- inferPat argPat
            resTy <- fresh
            unify tc (ITyFun argTy resTy)
            return (argBinds, resTy)
        Nothing -> throwE $ "Unknown constructor: " ++ T.unpack name
inferPat (PInfix p1 (VId op) p2) =
    inferPat (PCon (LongVId [] (VId op)) (Just (PTuple [p1, p2])))
inferPat (PTyped p _ty) = inferPat p -- TODO: check annotation
inferPat (PAs (VId name) p) = do
    (bs, t) <- inferPat p
    return ((T.unpack name, monoScheme t) : bs, t)
inferPat (PRecord fields hasWild) = do
    results <- mapM (\(Lab l, p) -> do (bs, t) <- inferPat p; return (bs, (T.unpack l, t))) fields
    let bindings = concatMap fst results
        fieldTypes = map snd results
    if hasWild
        then do a <- fresh; return (bindings, a) -- can't determine full record type
        else return (bindings, ITyRecord fieldTypes)
inferPat (PPos _ p) = inferPat p

------------------------------------------------------------------------
-- Declaration inference
------------------------------------------------------------------------

-- | Infer and register a declaration.
inferDec :: Dec -> InferM ()
-- val [rec] pat = exp
inferDec (DVal _ binds) = mapM_ inferValBind binds
-- fun clauses
inferDec (DFun _ binds) = mapM_ inferFunBind binds
-- type alias (just record in env for now)
inferDec (DType _) = return () -- TODO
-- datatype
inferDec (DDatatype datbinds) = mapM_ inferDatBind datbinds
-- exception
inferDec (DException _) = return () -- TODO
-- local
inferDec (DLocal decs1 decs2) = mapM_ inferDec decs1 >> mapM_ inferDec decs2
-- Sequential
inferDec (DSeq decs) = mapM_ inferDec decs
-- Fixity (no type implications)
inferDec (DInfix _ _) = return ()
inferDec (DInfixr _ _) = return ()
inferDec (DNonfix _) = return ()
inferDec (DOpen _) = return ()
inferDec (DPos _ d) = inferDec d
inferDec d = throwE $ "Unsupported declaration: " ++ show d

inferValBind :: ValBind -> InferM ()
inferValBind (ValBind isRec pat exp_) =
    if isRec
        then do
            -- For rec, bind the pattern variable first with a fresh type
            (bindings, _tp) <- inferPat pat
            modifyEnv (extendEnvMany bindings)
            te <- infer exp_
            scheme <- generalize te (isSyntacticValue exp_)
            -- Re-bind with the generalized scheme
            case bindings of
                [(x, _)] -> modifyEnv (extendEnv x scheme)
                _ -> return ()
        else do
            te <- infer exp_
            (bindings, tp) <- inferPat pat
            unify tp te
            let addBinding (x, _) = do
                    scheme <- generalize te (isSyntacticValue exp_)
                    modifyEnv (extendEnv x scheme)
            mapM_ addBinding bindings

inferFunBind :: FunBind -> InferM ()
inferFunBind (FunBind clauses@(FunClause (VId name) _ _ _ :| _)) = do
    -- Bind the function name with a fresh type for recursion
    funTy <- fresh
    let fname = T.unpack name
    modifyEnv (extendEnv fname (monoScheme funTy))
    -- Infer each clause
    mapM_ (inferFunClause funTy) clauses
    -- Generalize
    scheme <- generalize funTy True -- fun bindings are always values
    modifyEnv (extendEnv fname scheme)

inferFunClause :: ITy -> FunClause -> InferM ()
inferFunClause funTy (FunClause _ pats _retTy body) = do
    -- Build function type from patterns
    patResults <- mapM inferPat pats
    let bindings = concatMap fst patResults
        patTypes = map snd patResults
    -- Infer body with pattern bindings in scope
    bodyTy <- withBindings bindings (infer body)
    -- Unify: funTy = pat1 -> pat2 -> ... -> bodyTy
    let fullTy = foldr ITyFun bodyTy patTypes
    unify funTy fullTy

inferDatBind :: DatBind -> InferM ()
inferDatBind (DatBind tyvars (TyCon tcName) conbinds) = do
    -- Create fresh variables for the tyvar parameters
    freshTvs <- mapM (const fresh) tyvars
    let resultTy = ITyCon (T.unpack tcName) freshTvs
        tvUids = [u | ITyVar u <- freshTvs]
    -- Register each constructor
    mapM_ (registerCon tvUids resultTy) conbinds
  where
    registerCon tvUids resTy (ConBind (VId cname) Nothing) = do
        let scheme = Scheme tvUids resTy
            cn = T.unpack cname
            tc = T.unpack tcName
        modifyConEnv (extendCon cn scheme tc)
        modifyEnv (extendEnv cn scheme)
    registerCon tvUids resTy (ConBind (VId cname) (Just _argTy)) = do
        argFresh <- fresh -- TODO: translate surface type to internal type
        let conTy = ITyFun argFresh resTy
            scheme = Scheme tvUids conTy
            cn = T.unpack cname
            tc = T.unpack tcName
        modifyConEnv (extendCon cn scheme tc)
        modifyEnv (extendEnv cn scheme)

------------------------------------------------------------------------
-- Top-level entry point
------------------------------------------------------------------------

-- | Run inference on a whole program.
inferProgram :: InferState -> Program -> Either String InferState
inferProgram st (Program decs) = case runInfer st (mapM_ inferDec decs) of
    Left err -> Left err
    Right ((), st') -> Right st'
