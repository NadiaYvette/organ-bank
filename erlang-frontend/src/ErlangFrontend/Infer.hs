{- | Hindley-Milner type inference for Erlang.
Implements Algorithm W adapted for Erlang's dynamically-typed semantics.

Since Erlang is dynamically typed, inference is partial: we infer what
we can (arithmetic types, list/tuple structure, function arity) and
leave the rest as unresolved type variables.

Substitutions are threaded monadically (Jones-style): the current
substitution lives in InferState and is applied on demand via 'zonk'.
-}
module ErlangFrontend.Infer (
    InferState (..),
    initInferState,
    inferModule,
    erlTypeToOrganTy,
) where

import Control.Monad (zipWithM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.List (nub, (\\))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import ErlangFrontend.AST
import ErlangFrontend.Infer.Types
import OrganIR.Build qualified as IR
import OrganIR.Types qualified as IR

------------------------------------------------------------------------
-- Inference state and monad
------------------------------------------------------------------------

-- | Inference state: fresh variable supply + current substitution + env.
data InferState = InferState
    { isSupply :: !Int
    , isSubst :: !Subst
    , isEnv :: !(Map Text Scheme)
    }
    deriving (Show)

initInferState :: InferState
initInferState = InferState 0 emptySubst builtinEnv

-- | Built-in functions that Erlang programs commonly use.
builtinEnv :: Map Text Scheme
builtinEnv =
    Map.fromList
        [ ("erlang:length", Forall [0] (TFun [tyList (TVar 0)] tyInteger))
        , ("length", Forall [0] (TFun [tyList (TVar 0)] tyInteger))
        , ("erlang:hd", Forall [0] (TFun [tyList (TVar 0)] (TVar 0)))
        , ("hd", Forall [0] (TFun [tyList (TVar 0)] (TVar 0)))
        , ("erlang:tl", Forall [0] (TFun [tyList (TVar 0)] (tyList (TVar 0))))
        , ("tl", Forall [0] (TFun [tyList (TVar 0)] (tyList (TVar 0))))
        , ("erlang:abs", Forall [] (TFun [tyNumber] tyNumber))
        , ("abs", Forall [] (TFun [tyNumber] tyNumber))
        , ("erlang:self", Forall [] (TFun [] tyPid))
        , ("self", Forall [] (TFun [] tyPid))
        , ("erlang:spawn", Forall [] (TFun [TFun [] (TCon "any")] tyPid))
        , ("spawn", Forall [] (TFun [TFun [] (TCon "any")] tyPid))
        , ("erlang:is_integer", Forall [] (TFun [TCon "any"] tyBoolean))
        , ("is_integer", Forall [] (TFun [TCon "any"] tyBoolean))
        , ("erlang:is_float", Forall [] (TFun [TCon "any"] tyBoolean))
        , ("is_float", Forall [] (TFun [TCon "any"] tyBoolean))
        , ("erlang:is_atom", Forall [] (TFun [TCon "any"] tyBoolean))
        , ("is_atom", Forall [] (TFun [TCon "any"] tyBoolean))
        , ("erlang:is_list", Forall [] (TFun [TCon "any"] tyBoolean))
        , ("is_list", Forall [] (TFun [TCon "any"] tyBoolean))
        , ("erlang:is_tuple", Forall [] (TFun [TCon "any"] tyBoolean))
        , ("is_tuple", Forall [] (TFun [TCon "any"] tyBoolean))
        , ("erlang:is_pid", Forall [] (TFun [TCon "any"] tyBoolean))
        , ("is_pid", Forall [] (TFun [TCon "any"] tyBoolean))
        , ("erlang:element", Forall [0] (TFun [tyInteger, TTuple [TVar 0]] (TVar 0)))
        , ("element", Forall [0] (TFun [tyInteger, TTuple [TVar 0]] (TVar 0)))
        , ("erlang:tuple_size", Forall [] (TFun [TTuple []] tyInteger))
        , ("tuple_size", Forall [] (TFun [TTuple []] tyInteger))
        , ("io:format", Forall [] (TFun [tyString] tyAtom))
        , ("lists:reverse", Forall [0] (TFun [tyList (TVar 0)] (tyList (TVar 0))))
        , ("lists:map", Forall [0, 1] (TFun [TFun [TVar 0] (TVar 1), tyList (TVar 0)] (tyList (TVar 1))))
        , ("lists:filter", Forall [0] (TFun [TFun [TVar 0] tyBoolean, tyList (TVar 0)] (tyList (TVar 0))))
        , ("lists:foldl", Forall [0, 1] (TFun [TFun [TVar 0, TVar 1] (TVar 1), TVar 1, tyList (TVar 0)] (TVar 1)))
        , ("lists:foldr", Forall [0, 1] (TFun [TFun [TVar 0, TVar 1] (TVar 1), TVar 1, tyList (TVar 0)] (TVar 1)))
        ]

type InferM a = ExceptT String (State InferState) a

runInfer :: InferState -> InferM a -> (Either String a, InferState)
runInfer st m = runState (runExceptT m) st

------------------------------------------------------------------------
-- Monadic primitives
------------------------------------------------------------------------

-- | Generate a fresh unification variable.
fresh :: InferM ErlType
fresh = do
    u <- lift $ gets isSupply
    lift $ modify' $ \st -> st{isSupply = u + 100}
    -- Start at 100 to avoid clashing with builtin scheme vars (0,1,2...)
    pure (TVar u)

-- | Apply the current substitution to a type.
zonk :: ErlType -> InferM ErlType
zonk ty = do
    s <- lift $ gets isSubst
    pure (apply s ty)

-- | Extend the substitution.
addSubst :: Int -> ErlType -> InferM ()
addSubst u t = lift $ modify' $ \st -> st{isSubst = Map.insert u t (isSubst st)}

------------------------------------------------------------------------
-- Environment operations
------------------------------------------------------------------------

getEnv :: InferM (Map Text Scheme)
getEnv = lift $ gets isEnv

modifyEnv :: (Map Text Scheme -> Map Text Scheme) -> InferM ()
modifyEnv f = lift $ modify' $ \st -> st{isEnv = f (isEnv st)}

-- | Run an action with extra bindings, then restore the original env.
withBindings :: [(Text, Scheme)] -> InferM a -> InferM a
withBindings bindings action = do
    oldEnv <- getEnv
    modifyEnv (Map.union (Map.fromList bindings))
    result <- action
    modifyEnv (const oldEnv)
    pure result

-- | Look up a variable in the environment.
lookupVar :: Text -> InferM (Maybe Scheme)
lookupVar x = Map.lookup x <$> getEnv

------------------------------------------------------------------------
-- Unification
------------------------------------------------------------------------

-- | Unify two types under the current substitution.
unify :: ErlType -> ErlType -> InferM ()
unify t1 t2 = do
    t1' <- zonk t1
    t2' <- zonk t2
    unify' t1' t2'

unify' :: ErlType -> ErlType -> InferM ()
unify' (TVar u) t = bindVar u t
unify' t (TVar u) = bindVar u t
unify' (TCon a) (TCon b)
    | a == b = pure ()
    -- number subsumes integer and float
    | a == "number" && (b == "integer" || b == "float") = pure ()
    | b == "number" && (a == "integer" || a == "float") = pure ()
    | otherwise = throwE $ "Cannot unify " ++ T.unpack a ++ " with " ++ T.unpack b
unify' (TApp c1 as1) (TApp c2 as2)
    | c1 == c2 && length as1 == length as2 = zipWithM_ unify as1 as2
    | otherwise = throwE $ "Cannot unify " ++ T.unpack c1 ++ " with " ++ T.unpack c2
unify' (TFun args1 r1) (TFun args2 r2)
    | length args1 == length args2 = zipWithM_ unify args1 args2 >> unify r1 r2
    | otherwise = throwE "Function arity mismatch"
unify' (TTuple ts1) (TTuple ts2)
    | length ts1 == length ts2 = zipWithM_ unify ts1 ts2
    | otherwise = throwE "Tuple arity mismatch"
-- If one side is "any", just succeed (Erlang is dynamic)
unify' (TCon "any") _ = pure ()
unify' _ (TCon "any") = pure ()
unify' t1 t2 = throwE $ "Cannot unify " ++ show t1 ++ " with " ++ show t2

bindVar :: Int -> ErlType -> InferM ()
bindVar u t
    | t == TVar u = pure ()
    | u `elem` ftv t = throwE $ "Occurs check: t" ++ show u ++ " in " ++ show t
    | otherwise = addSubst u t

------------------------------------------------------------------------
-- Instantiation & Generalization
------------------------------------------------------------------------

-- | Instantiate a scheme with fresh variables.
instantiate :: Scheme -> InferM ErlType
instantiate (Forall [] t) = pure t
instantiate (Forall vs t) = do
    freshVars <- mapM (const fresh) vs
    let mapping = Map.fromList (zip vs freshVars)
    pure (applyMapping mapping t)
  where
    applyMapping m (TVar u) = case Map.lookup u m of Just t' -> t'; Nothing -> TVar u
    applyMapping _ t'@(TCon _) = t'
    applyMapping m (TApp c as) = TApp c (map (applyMapping m) as)
    applyMapping m (TFun as r) = TFun (map (applyMapping m) as) (applyMapping m r)
    applyMapping m (TTuple ts) = TTuple (map (applyMapping m) ts)

-- | Generalize a type over variables not free in the environment.
generalize :: ErlType -> InferM Scheme
generalize t = do
    t' <- zonk t
    env <- getEnv
    s <- lift $ gets isSubst
    let envFree = concatMap (ftvScheme . applyScheme s) (Map.elems env)
        tyFree = nub (ftv t')
        genVars = tyFree \\ envFree
    pure (Forall genVars t')

------------------------------------------------------------------------
-- Guard constraint extraction
------------------------------------------------------------------------

-- | Extract type constraints from guard tests.
-- is_integer(X) means X : integer(), etc.
guardConstraints :: [Expr] -> [(Text, ErlType)]
guardConstraints = concatMap guardTest

guardTest :: Expr -> [(Text, ErlType)]
guardTest = \case
    ECall (EAtom "is_integer") [EVar v] -> [(v, tyInteger)]
    ECall (EAtom "is_float") [EVar v] -> [(v, tyFloat)]
    ECall (EAtom "is_atom") [EVar v] -> [(v, tyAtom)]
    ECall (EAtom "is_list") [EVar v] -> [(v, tyList (TVar (-1)))] -- placeholder
    ECall (EAtom "is_tuple") [EVar v] -> [(v, TTuple [])]
    ECall (EAtom "is_boolean") [EVar v] -> [(v, tyBoolean)]
    ECall (EAtom "is_pid") [EVar v] -> [(v, tyPid)]
    ECall (EAtom "is_number") [EVar v] -> [(v, tyNumber)]
    _ -> []

-- | Apply guard constraints to the environment.
applyGuardConstraints :: Maybe GuardSeq -> InferM ()
applyGuardConstraints Nothing = pure ()
applyGuardConstraints (Just gs) = do
    -- Each disjunction branch: pick constraints that appear in ALL branches
    -- For simplicity, take constraints from the first branch (conjunction)
    case gs of
        (conj : _) -> do
            let constraints = guardConstraints conj
            mapM_ applyConstraint constraints
        [] -> pure ()
  where
    applyConstraint (v, ty) = do
        ms <- lookupVar v
        case ms of
            Just s -> do
                t <- instantiate s
                -- Try to unify; if it fails, silently ignore (dynamic typing)
                catchE (unify t ty) (\_ -> pure ())
            Nothing -> pure ()

------------------------------------------------------------------------
-- Expression inference
------------------------------------------------------------------------

-- | Infer the type of an expression.
inferExpr :: Expr -> InferM ErlType
-- Literals
inferExpr (EAtom "true") = pure tyBoolean
inferExpr (EAtom "false") = pure tyBoolean
inferExpr (EAtom _) = pure tyAtom
inferExpr (EInt _) = pure tyInteger
inferExpr (EFloat _) = pure tyFloat
inferExpr (EString _) = pure tyString
inferExpr (EChar _) = pure tyInteger -- Erlang chars are integers
-- Variables
inferExpr (EVar v) = do
    ms <- lookupVar v
    case ms of
        Just s -> instantiate s
        Nothing -> fresh -- unknown variable, give it a fresh type
-- Tuples
inferExpr (ETuple es) = TTuple <$> mapM inferExpr es
-- Lists
inferExpr (EList [] Nothing) = tyList <$> fresh
inferExpr (EList (e : es) Nothing) = do
    t <- inferExpr e
    mapM_ (\e' -> do t' <- inferExpr e'; catchE (unify t t') (\_ -> pure ())) es
    pure (tyList t)
inferExpr (EList es (Just tl)) = do
    elemTy <- fresh
    mapM_ (\e -> do t <- inferExpr e; catchE (unify elemTy t) (\_ -> pure ())) es
    tlTy <- inferExpr tl
    catchE (unify (tyList elemTy) tlTy) (\_ -> pure ())
    pure (tyList elemTy)
-- Binary operators
inferExpr (EBinOp op l r) = inferBinOp op l r
-- Unary operators
inferExpr (EUnOp op e) = inferUnOp op e
-- Function calls
inferExpr (ECall (ERemote (EAtom m) (EAtom f)) args) = do
    let qualName = m <> ":" <> f
    ms <- lookupVar qualName
    case ms of
        Just s -> do
            ft <- instantiate s
            inferCallWithType ft args
        Nothing -> do
            -- Unknown remote call: infer arg types, return fresh
            mapM_ inferExpr args
            fresh
inferExpr (ECall (EAtom f) args) = do
    ms <- lookupVar f
    case ms of
        Just s -> do
            ft <- instantiate s
            inferCallWithType ft args
        Nothing -> do
            mapM_ inferExpr args
            fresh
inferExpr (ECall (EVar f) args) = do
    ms <- lookupVar f
    case ms of
        Just s -> do
            ft <- instantiate s
            inferCallWithType ft args
        Nothing -> do
            mapM_ inferExpr args
            fresh
inferExpr (ECall _ args) = do
    mapM_ inferExpr args
    fresh
-- Fun references
inferExpr (EFun _name arity) = do
    argTys <- mapM (const fresh) [1 .. arity]
    retTy <- fresh
    pure (TFun argTys retTy)
inferExpr (EFunRef _ _ arity) = do
    argTys <- mapM (const fresh) [1 .. arity]
    retTy <- fresh
    pure (TFun argTys retTy)
-- Lambda
inferExpr (ELambda clauses) = inferLambdaClauses clauses
-- Case
inferExpr (ECase scrut clauses) = do
    scrutTy <- inferExpr scrut
    resultTy <- fresh
    mapM_ (inferCaseClause scrutTy resultTy) clauses
    pure resultTy
-- If
inferExpr (EIf clauses) = do
    resultTy <- fresh
    mapM_ (inferIfClause resultTy) clauses
    pure resultTy
-- Receive
inferExpr (EReceive clauses _after) = do
    resultTy <- fresh
    -- Message type is unknown
    msgTy <- fresh
    mapM_ (inferCaseClause msgTy resultTy) clauses
    pure resultTy
-- Begin
inferExpr (EBegin body) = inferBody body
-- Try
inferExpr (ETry body _ofClauses _catchClauses _afterBody) = inferBody body
-- Catch
inferExpr (ECatch e) = inferExpr e
-- List comprehension
inferExpr (EListComp e _quals) = do
    t <- inferExpr e
    pure (tyList t)
-- Match
inferExpr (EMatch _l r) = inferExpr r
-- Send
inferExpr (ESend _pid msg) = inferExpr msg
-- Remote
inferExpr (ERemote _ _) = fresh

-- | Infer a function call given the function type.
inferCallWithType :: ErlType -> [Expr] -> InferM ErlType
inferCallWithType ft args = do
    argTys <- mapM inferExpr args
    retTy <- fresh
    catchE (unify ft (TFun argTys retTy)) (\_ -> pure ())
    zonk retTy

-- | Infer a binary operator expression.
inferBinOp :: Text -> Expr -> Expr -> InferM ErlType
inferBinOp op l r
    -- Arithmetic: +, -, *, div, rem → integer; / → float
    | op `elem` ["+", "-", "*"] = do
        tl <- inferExpr l
        tr <- inferExpr r
        -- Both integer → integer; otherwise number
        tl' <- zonk tl
        tr' <- zonk tr
        case (tl', tr') of
            (TCon "integer", TCon "integer") -> pure tyInteger
            (TCon "float", _) -> pure tyFloat
            (_, TCon "float") -> pure tyFloat
            _ -> do
                catchE (unify tl tyNumber) (\_ -> pure ())
                catchE (unify tr tyNumber) (\_ -> pure ())
                pure tyNumber
    | op == "/" = do
        tl <- inferExpr l
        tr <- inferExpr r
        catchE (unify tl tyNumber) (\_ -> pure ())
        catchE (unify tr tyNumber) (\_ -> pure ())
        pure tyFloat
    | op `elem` ["div", "rem", "band", "bor", "bxor", "bsl", "bsr"] = do
        tl <- inferExpr l
        tr <- inferExpr r
        catchE (unify tl tyInteger) (\_ -> pure ())
        catchE (unify tr tyInteger) (\_ -> pure ())
        pure tyInteger
    -- Comparison operators → boolean
    | op `elem` ["==", "/=", "=:=", "=/=", "<", ">", ">=", "=<"] = do
        _ <- inferExpr l
        _ <- inferExpr r
        pure tyBoolean
    -- Boolean operators
    | op `elem` ["andalso", "orelse", "and", "or", "xor"] = do
        tl <- inferExpr l
        tr <- inferExpr r
        catchE (unify tl tyBoolean) (\_ -> pure ())
        catchE (unify tr tyBoolean) (\_ -> pure ())
        pure tyBoolean
    -- List append
    | op == "++" = do
        tl <- inferExpr l
        tr <- inferExpr r
        catchE (unify tl tr) (\_ -> pure ())
        pure tl
    -- List subtract
    | op == "--" = do
        tl <- inferExpr l
        _ <- inferExpr r
        pure tl
    -- Fallback
    | otherwise = do
        _ <- inferExpr l
        _ <- inferExpr r
        fresh

-- | Infer a unary operator expression.
inferUnOp :: Text -> Expr -> InferM ErlType
inferUnOp op e
    | op == "-" = do
        t <- inferExpr e
        catchE (unify t tyNumber) (\_ -> pure ())
        pure t
    | op == "not" = do
        t <- inferExpr e
        catchE (unify t tyBoolean) (\_ -> pure ())
        pure tyBoolean
    | op == "bnot" = do
        t <- inferExpr e
        catchE (unify t tyInteger) (\_ -> pure ())
        pure tyInteger
    | otherwise = inferExpr e

------------------------------------------------------------------------
-- Pattern inference
------------------------------------------------------------------------

-- | Infer the type of a pattern, returning variable bindings and the pattern type.
inferPat :: Pat -> InferM ([(Text, Scheme)], ErlType)
inferPat PWild = do a <- fresh; pure ([], a)
inferPat (PVar v) = do
    a <- fresh
    pure ([(v, monoScheme a)], a)
inferPat (PAtom "true") = pure ([], tyBoolean)
inferPat (PAtom "false") = pure ([], tyBoolean)
inferPat (PAtom _) = pure ([], tyAtom)
inferPat (PInt _) = pure ([], tyInteger)
inferPat (PFloat _) = pure ([], tyFloat)
inferPat (PString _) = pure ([], tyString)
inferPat (PChar _) = pure ([], tyInteger)
inferPat (PTuple ps) = do
    results <- mapM inferPat ps
    let bindings = concatMap fst results
        types = map snd results
    pure (bindings, TTuple types)
inferPat (PList [] Nothing) = do a <- fresh; pure ([], tyList a)
inferPat (PList (p : ps) Nothing) = do
    (bs1, t) <- inferPat p
    bss <- mapM (\p' -> do (bs, t') <- inferPat p'; catchE (unify t t') (\_ -> pure ()); pure bs) ps
    pure (bs1 ++ concat bss, tyList t)
inferPat (PList ps (Just tl)) = do
    elemTy <- fresh
    bss <- mapM (\p -> do (bs, t) <- inferPat p; catchE (unify elemTy t) (\_ -> pure ()); pure bs) ps
    (bsTl, tlTy) <- inferPat tl
    catchE (unify (tyList elemTy) tlTy) (\_ -> pure ())
    pure (concat bss ++ bsTl, tyList elemTy)
inferPat (PCons h t) = do
    (bsH, hTy) <- inferPat h
    (bsT, tTy) <- inferPat t
    catchE (unify (tyList hTy) tTy) (\_ -> pure ())
    pure (bsH ++ bsT, tyList hTy)
inferPat (PMatch p1 p2) = do
    (bs1, t1) <- inferPat p1
    (bs2, t2) <- inferPat p2
    catchE (unify t1 t2) (\_ -> pure ())
    pure (bs1 ++ bs2, t1)
inferPat (PBinOp _ p1 p2) = do
    (bs1, _) <- inferPat p1
    (bs2, t2) <- inferPat p2
    pure (bs1 ++ bs2, t2)

------------------------------------------------------------------------
-- Clause inference
------------------------------------------------------------------------

-- | Infer a case clause, unifying with expected scrutinee and result types.
inferCaseClause :: ErlType -> ErlType -> CaseClause -> InferM ()
inferCaseClause scrutTy resTy (CaseClause pat guard_ body) = do
    (bindings, patTy) <- inferPat pat
    catchE (unify scrutTy patTy) (\_ -> pure ())
    withBindings bindings $ do
        applyGuardConstraints guard_
        bodyTy <- inferBody body
        catchE (unify resTy bodyTy) (\_ -> pure ())

-- | Infer an if clause.
inferIfClause :: ErlType -> CaseClause -> InferM ()
inferIfClause resTy (CaseClause _ _guard body) = do
    bodyTy <- inferBody body
    catchE (unify resTy bodyTy) (\_ -> pure ())

-- | Infer a lambda from its clauses.
inferLambdaClauses :: [FunClause] -> InferM ErlType
inferLambdaClauses [] = fresh
inferLambdaClauses clauses@(FunClause pats _ _ : _) = do
    let arity = length pats
    argTys <- mapM (const fresh) [1 .. arity]
    retTy <- fresh
    mapM_ (inferFunClause argTys retTy) clauses
    argTys' <- mapM zonk argTys
    retTy' <- zonk retTy
    pure (TFun argTys' retTy')

-- | Infer a single function clause.
inferFunClause :: [ErlType] -> ErlType -> FunClause -> InferM ()
inferFunClause argTys retTy (FunClause pats guard_ body) = do
    allBindings <- zipWithM_ (\argTy pat -> do
        (bindings, patTy) <- inferPat pat
        catchE (unify argTy patTy) (\_ -> pure ())
        modifyEnv (Map.union (Map.fromList bindings))
        ) argTys pats
    allBindings `seq` pure ()
    applyGuardConstraints guard_
    bodyTy <- inferBody body
    catchE (unify retTy bodyTy) (\_ -> pure ())

-- | Infer the type of a body (sequence of expressions, last is the result).
inferBody :: [Expr] -> InferM ErlType
inferBody [] = fresh
inferBody [e] = inferExpr e
inferBody (e : es) = inferExpr e >> inferBody es

------------------------------------------------------------------------
-- Module-level inference
------------------------------------------------------------------------

-- | Result of module-level inference: map from function name/arity to inferred type.
type InferResult = Map Text ErlType

-- | Infer types for all functions in a module.
inferModule :: [Form] -> InferResult
inferModule forms =
    let st = initInferState{isSupply = 100}
        (result, _finalSt) = runInfer st (inferForms forms)
    in  case result of
            Right r -> r
            Left _ -> Map.empty -- Inference failed, return empty (everything TAny)

-- | Infer types for all forms, collecting function types.
inferForms :: [Form] -> InferM InferResult
inferForms forms = do
    -- First pass: register all function names with fresh types (for mutual recursion)
    mapM_ registerFun forms
    -- Second pass: infer function bodies
    results <- mapM inferForm forms
    pure (Map.unions results)

-- | Register a function with a fresh type variable for forward references.
registerFun :: Form -> InferM ()
registerFun (FFun fname clauses) = do
    let arity = case clauses of
            (FunClause pats _ _ : _) -> length pats
            _ -> 0
    argTys <- mapM (const fresh) [1 .. arity]
    retTy <- fresh
    let funTy = TFun argTys retTy
    modifyEnv (Map.insert fname (monoScheme funTy))
registerFun _ = pure ()

-- | Infer types for a single form.
inferForm :: Form -> InferM InferResult
inferForm (FFun fname clauses) = do
    let arity = case clauses of
            (FunClause pats _ _ : _) -> length pats
            _ -> 0
    -- Get the pre-registered type
    ms <- lookupVar fname
    case ms of
        Just s -> do
            funTy <- instantiate s
            case funTy of
                TFun argTys retTy -> do
                    -- Save env before inferring clauses
                    oldEnv <- getEnv
                    mapM_ (inferFunClause argTys retTy) clauses
                    -- Restore env (function body bindings are local)
                    modifyEnv (const oldEnv)
                    -- Zonk the function type
                    argTys' <- mapM zonk argTys
                    retTy' <- zonk retTy
                    let finalTy = TFun argTys' retTy'
                    -- Generalize and re-register
                    scheme <- generalize finalTy
                    modifyEnv (Map.insert fname scheme)
                    let key = fname <> "/" <> T.pack (show arity)
                    pure (Map.singleton key finalTy)
                _ -> do
                    -- Shouldn't happen, but handle gracefully
                    argTys <- mapM (const fresh) [1 .. arity]
                    retTy <- fresh
                    mapM_ (inferFunClause argTys retTy) clauses
                    argTys' <- mapM zonk argTys
                    retTy' <- zonk retTy
                    let key = fname <> "/" <> T.pack (show arity)
                    pure (Map.singleton key (TFun argTys' retTy'))
        Nothing -> pure Map.empty
inferForm _ = pure Map.empty

------------------------------------------------------------------------
-- Conversion to OrganIR types
------------------------------------------------------------------------

-- | Convert an inferred ErlType to an OrganIR Ty.
-- Unresolved type variables become TAny.
erlTypeToOrganTy :: Subst -> ErlType -> IR.Ty
erlTypeToOrganTy s t = go (apply s t)
  where
    go :: ErlType -> IR.Ty
    go (TVar _) = IR.TAny -- unresolved → dynamic
    go (TCon "any") = IR.TAny
    go (TCon "integer") = IR.TCon (IR.localName "integer")
    go (TCon "float") = IR.TCon (IR.localName "float")
    go (TCon "number") = IR.TCon (IR.localName "number")
    go (TCon "atom") = IR.TCon (IR.localName "atom")
    go (TCon "string") = IR.TCon (IR.localName "string")
    go (TCon "boolean") = IR.TCon (IR.localName "boolean")
    go (TCon "pid") = IR.TCon (IR.localName "pid")
    go (TCon "reference") = IR.TCon (IR.localName "reference")
    go (TCon c) = IR.TCon (IR.localName c)
    go (TApp "list" [elem_]) = IR.TApp (IR.localName "list") [go elem_]
    go (TApp c args) = IR.TApp (IR.localName c) (map go args)
    go (TFun args ret) =
        IR.TFn
            (map (IR.FnArg Nothing . go) args)
            (IR.EffectRow [] Nothing)
            (go ret)
    go (TTuple ts) =
        IR.TApp (IR.localName "tuple") (map go ts)
