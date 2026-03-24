{- | Translate Erlang AST to OrganIR.
Strategy: group function clauses by name/arity → one Definition per function.
Guard sequences become conjunctive/disjunctive tests via eAnd/eOr.
-}
module ErlangFrontend.ToOrganIR (emitErlangIR) where

import Data.Text (Text)
import Data.Text qualified as T
import ErlangFrontend.AST
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR

-- | Emit OrganIR JSON for an Erlang program.
emitErlangIR :: String -> FilePath -> [Form] -> Text
emitErlangIR modName srcFile forms =
    renderOrganIR $
        IR.simpleOrganIR IR.LErlang "erlang-frontend-0.1" (T.pack modName) srcFile $
            concatMap formToDefs forms

formToDefs :: Form -> [IR.Definition]
formToDefs = \case
    FFun fname clauses ->
        let arity = case clauses of
                (FunClause pats _ _ : _) -> length pats
                _ -> 0
            params = map (\i -> T.pack ("_A" ++ show i)) [1 .. arity]
            body = clausesToExpr params clauses
            expr = IR.ELam (map (\p -> IR.LamParam (IR.name p) Nothing) params) body
         in [IR.funDefNA fname arity expr]
    _ -> []

{- | Convert function clauses to an expression.
Multiple clauses → try each in sequence (eOr pattern).
-}
clausesToExpr :: [Text] -> [FunClause] -> IR.Expr
clausesToExpr params [c] = singleClause params c
clausesToExpr params cs = IR.eOr (map (singleClause params) cs)

singleClause :: [Text] -> FunClause -> IR.Expr
singleClause params (FunClause pats guard_ body) =
    let bindings = zipWith (unifyPat . IR.eVar) params pats
        guardExpr = guardToExpr guard_
        bodyExpr = IR.eSeq (map exprToIR body)
     in wrapBindings bindings (maybe bodyExpr (\g -> IR.eGuarded g bodyExpr (IR.ELit (IR.LitBool False))) guardExpr)

-- | Generate a unification expression for pattern matching.
unifyPat :: IR.Expr -> Pat -> IR.Expr
unifyPat scrut = \case
    PWild -> IR.ELit (IR.LitBool True)
    PVar v -> IR.EApp (IR.eVar "match") [scrut, IR.eVar v]
    PAtom a -> IR.EApp (IR.eVar "=:=") [scrut, IR.ECon (IR.localName a) []]
    PInt n -> IR.EApp (IR.eVar "=:=") [scrut, IR.eInt n]
    PFloat d -> IR.EApp (IR.eVar "=:=") [scrut, IR.eFloat d]
    PString s -> IR.EApp (IR.eVar "=:=") [scrut, IR.eString s]
    PChar c -> IR.EApp (IR.eVar "=:=") [scrut, IR.eString (T.singleton c)]
    PTuple ps -> IR.EApp (IR.eVar "match_tuple") [scrut, IR.ETuple (map patToIR ps)]
    PList ps Nothing -> IR.EApp (IR.eVar "match_list") [scrut, IR.EList (map patToIR ps)]
    PList ps (Just tl) ->
        IR.EApp (IR.eVar "match_list_tail") [scrut, IR.EList (map patToIR ps), patToIR tl]
    PCons h t -> IR.EApp (IR.eVar "match_cons") [scrut, patToIR h, patToIR t]
    PMatch p1 p2 -> IR.eAnd [unifyPat scrut p1, unifyPat scrut p2]
    PBinOp{} -> IR.ELit (IR.LitBool True) -- skip complex patterns

-- | Convert a pattern to an expression (for use in match arguments).
patToIR :: Pat -> IR.Expr
patToIR = \case
    PAtom a -> IR.ECon (IR.localName a) []
    PVar v -> IR.eVar v
    PWild -> IR.eVar "_"
    PInt n -> IR.eInt n
    PFloat d -> IR.eFloat d
    PString s -> IR.eString s
    PChar c -> IR.eString (T.singleton c)
    PTuple ps -> IR.ETuple (map patToIR ps)
    PList ps Nothing -> IR.EList (map patToIR ps)
    PList ps (Just tl) ->
        foldr (\h t -> IR.ECon (IR.localName "[|]") [patToIR h, t]) (patToIR tl) ps
    PCons h t -> IR.ECon (IR.localName "[|]") [patToIR h, patToIR t]
    PMatch _ p -> patToIR p
    PBinOp op l r -> IR.EApp (IR.eVar op) [patToIR l, patToIR r]

{- | Convert guard sequences to an expression.
Outer list = disjunction (;), inner list = conjunction (,).
-}
guardToExpr :: Maybe GuardSeq -> Maybe IR.Expr
guardToExpr Nothing = Nothing
guardToExpr (Just gs) = Just $ IR.eOr (map (IR.eAnd . map exprToIR) gs)

-- | Wrap bindings around a body.
wrapBindings :: [IR.Expr] -> IR.Expr -> IR.Expr
wrapBindings [] body = body
wrapBindings bs body = IR.eAnd (bs ++ [body])

-- | Translate an expression to OrganIR.
exprToIR :: Expr -> IR.Expr
exprToIR = \case
    EAtom a -> IR.ECon (IR.localName a) []
    EVar v -> IR.eVar (T.toLower v)
    EInt n -> IR.eInt n
    EFloat d -> IR.eFloat d
    EString s -> IR.eString s
    EChar c -> IR.eString (T.singleton c)
    ETuple es -> IR.ETuple (map exprToIR es)
    EList es Nothing -> IR.EList (map exprToIR es)
    EList es (Just tl) ->
        foldr (\h t -> IR.ECon (IR.localName "[|]") [exprToIR h, t]) (exprToIR tl) es
    EBinOp op l r -> IR.EApp (IR.eVar op) [exprToIR l, exprToIR r]
    EUnOp op e -> IR.EApp (IR.eVar op) [exprToIR e]
    ECall (ERemote m f) args -> IR.EApp (IR.EApp (IR.eVar ":") [exprToIR m, exprToIR f]) (map exprToIR args)
    ECall f args -> IR.EApp (exprToIR f) (map exprToIR args)
    ERemote m f -> IR.EApp (IR.eVar ":") [exprToIR m, exprToIR f]
    EFun fname arity -> IR.eVar (fname <> "/" <> T.pack (show arity))
    EFunRef m f arity -> IR.EApp (IR.eVar ":") [exprToIR m, IR.eVar (T.pack (show f) <> "/" <> T.pack (show arity))]
    ELambda clauses ->
        let arity = case clauses of
                (FunClause pats _ _ : _) -> length pats
                _ -> 0
            params = map (\i -> T.pack ("_L" ++ show i)) [1 .. arity]
         in IR.ELam (map (\p -> IR.LamParam (IR.name p) Nothing) params) (clausesToExpr params clauses)
    ECase scrut clauses ->
        let tmp = "_case"
         in IR.eLet1 tmp (exprToIR scrut) (caseToIR tmp clauses)
    EIf clauses -> ifClausesToIR clauses
    EReceive clauses after_ ->
        let recv = IR.EApp (IR.eVar "receive") [IR.ELam [] (caseToIR "_msg" clauses)]
         in case after_ of
                Nothing -> recv
                Just (timeout, body) ->
                    IR.EApp (IR.eVar "receive_after") [exprToIR timeout, IR.ELam [] (IR.eSeq (map exprToIR body))]
    EBegin body -> IR.eSeq (map exprToIR body)
    ETry body ofClauses catchClauses afterBody ->
        IR.EApp
            (IR.eVar "try")
            [ IR.ELam [] (IR.eSeq (map exprToIR body))
            , IR.ELam [] (if null ofClauses then IR.ELit (IR.LitBool True) else caseToIR "_try" ofClauses)
            , IR.ELam [] (if null catchClauses then IR.ELit (IR.LitBool True) else caseToIR "_catch" catchClauses)
            , IR.ELam [] (IR.eSeq (map exprToIR afterBody))
            ]
    ECatch e -> IR.EApp (IR.eVar "catch") [exprToIR e]
    EListComp e quals ->
        IR.EApp (IR.eVar "list_comp") [IR.ELam [] (exprToIR e), IR.EList (map qualToIR quals)]
    EMatch l r -> IR.EApp (IR.eVar "match") [exprToIR l, exprToIR r]
    ESend pid msg -> IR.EApp (IR.eVar "!") [exprToIR pid, exprToIR msg]

-- | Convert case clauses to an eOr chain.
caseToIR :: Text -> [CaseClause] -> IR.Expr
caseToIR scrut clauses = IR.eOr (map (ccToIR scrut) clauses)

ccToIR :: Text -> CaseClause -> IR.Expr
ccToIR scrut (CaseClause pat guard_ body) =
    let binding = unifyPat (IR.eVar scrut) pat
        guardExpr = guardToExpr guard_
        bodyExpr = IR.eSeq (map exprToIR body)
     in wrapBindings [binding] (maybe bodyExpr (\g -> IR.eGuarded g bodyExpr (IR.ELit (IR.LitBool False))) guardExpr)

-- | Convert if clauses to a chain of guarded expressions.
ifClausesToIR :: [CaseClause] -> IR.Expr
ifClausesToIR = IR.eOr . map ifClauseToIR

ifClauseToIR :: CaseClause -> IR.Expr
ifClauseToIR (CaseClause _ guard_ body) =
    let bodyExpr = IR.eSeq (map exprToIR body)
     in case guardToExpr guard_ of
            Nothing -> bodyExpr
            Just g -> IR.eGuarded g bodyExpr (IR.ELit (IR.LitBool False))

qualToIR :: Qualifier -> IR.Expr
qualToIR = \case
    QGenerator pat gen -> IR.EApp (IR.eVar "generator") [IR.ELam [] (patToIR pat), exprToIR gen]
    QFilter e -> IR.EApp (IR.eVar "filter") [IR.ELam [] (exprToIR e)]
