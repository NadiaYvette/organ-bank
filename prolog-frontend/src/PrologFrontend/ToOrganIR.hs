{- | Translate Prolog clauses to OrganIR.

Strategy: group clauses by predicate name/arity → one Definition per predicate.
Multiple clauses become case branches (disjunction).
Body goals become sequenced applications (conjunction).
-}
module PrologFrontend.ToOrganIR (emitPrologIR) where

import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build
import OrganIR.Json (renderOrganIR)
import OrganIR.Types
import PrologFrontend.Term

-- | Emit OrganIR JSON for a Prolog program.
emitPrologIR :: String -> FilePath -> [Sentence] -> Text
emitPrologIR modName srcFile sentences =
    renderOrganIR $
        simpleOrganIR LProlog "prolog-frontend-0.1" (T.pack modName) srcFile $
            concatMap predToDef (Map.toList grouped)
  where
    clauses = [c | c@(SClause _ _) <- sentences]
    grouped = groupClauses clauses

-- | Group clauses by predicate name/arity.
groupClauses :: [Sentence] -> Map PredKey [(Term, Maybe Term)]
groupClauses = foldl addClause Map.empty
  where
    addClause m (SClause head_ body) = case predKey head_ of
        Just k -> Map.insertWith (++) k [(head_, body)] m
        Nothing -> m
    addClause m _ = m

-- | Generate a Definition for one predicate.
predToDef :: (PredKey, [(Term, Maybe Term)]) -> [Definition]
predToDef ((fname, arity), clauses_) =
    let params = map (\i -> T.pack ("_A" ++ show i)) [1 .. arity]
        body = case clauses_ of
            [(head_, mbody)] -> singleClause params head_ mbody
            _ -> multiClause params clauses_
        expr = ELam (map (\p -> LamParam (name p) Nothing) params) body
     in [funDef fname TAny expr arity]

-- | Single clause: bind head args, then translate body.
singleClause :: [Text] -> Term -> Maybe Term -> Expr
singleClause params head_ mbody =
    let headArgs = termArgs head_
        bindings = zipWith matchArg params headArgs
        bodyExpr = maybe (ELit (LitBool True)) goalToExpr mbody
     in wrapBindings bindings bodyExpr

-- | Multiple clauses: each clause is a branch tried in order (disjunction).
multiClause :: [Text] -> [(Term, Maybe Term)] -> Expr
multiClause params clauses_ =
    eOr (map (clauseToExpr params) clauses_)

clauseToExpr :: [Text] -> (Term, Maybe Term) -> Expr
clauseToExpr params (head_, mbody) =
    let headArgs = termArgs head_
        bindings = zipWith matchArg params headArgs
        bodyExpr = maybe (ELit (LitBool True)) goalToExpr mbody
     in wrapBindings bindings bodyExpr

-- | Wrap unify-bindings around a body expression.
wrapBindings :: [(Text, Term)] -> Expr -> Expr
wrapBindings [] body = body
wrapBindings bs body =
    ELet
        (map (\(p, t) -> LetBind (name p) Nothing (EApp (eVar "unify") [eVar p, termToExpr t])) bs)
        body

-- | Extract the arguments from a compound head, or empty for atoms.
termArgs :: Term -> [Term]
termArgs = \case
    TmCompound _ args -> args
    _ -> []

{- | Generate a param/arg unification pair. If the arg is just a variable
matching the param, skip it; otherwise generate a unify call.
-}
matchArg :: Text -> Term -> (Text, Term)
matchArg param arg = (param, arg)

-- | Translate a goal term to an expression.
goalToExpr :: Term -> Expr
goalToExpr term = case term of
    -- Conjunction: (A, B) → sequence
    _ | not (null conj) && length conj > 1 -> eSeq (map goalToExpr conj)
      where
        conj = flattenConj term
    -- Disjunction: (A ; B) → or
    _ | not (null disj) && length disj > 1 -> eOr (map goalToExpr disj)
      where
        disj = flattenDisj term
    -- If-then: (Cond -> Then)
    TmCompound "->" [cond, then_] ->
        eIf (goalToExpr cond) (goalToExpr then_) (ELit (LitBool False))
    -- If-then-else: (Cond -> Then ; Else)
    TmCompound ";" [TmCompound "->" [cond, then_], else_] ->
        eIf (goalToExpr cond) (goalToExpr then_) (goalToExpr else_)
    -- Negation
    TmCompound "\\+" [g] -> EApp (eVar "not") [goalToExpr g]
    TmCompound "not" [g] -> EApp (eVar "not") [goalToExpr g]
    -- Cut
    TmAtom "!" -> eVar "!"
    -- true/fail
    TmAtom "true" -> ELit (LitBool True)
    TmAtom "fail" -> ELit (LitBool False)
    TmAtom "false" -> ELit (LitBool False)
    -- Unification: X = Y
    TmCompound "=" [l, r] -> EApp (eVar "unify") [termToExpr l, termToExpr r]
    -- Arithmetic: X is Expr
    TmCompound "is" [l, r] -> EApp (eVar "is") [termToExpr l, termToExpr r]
    -- Comparison operators
    TmCompound op [l, r]
        | op `elem` ["<", ">", ">=", "=<", "=:=", "=\\=", "==", "\\==", "@<", "@>", "@>=", "@=<"] ->
            EApp (eVar op) [termToExpr l, termToExpr r]
    -- Assert/retract
    TmCompound "assert" [t] -> EApp (eVar "assert") [termToExpr t]
    TmCompound "retract" [t] -> EApp (eVar "retract") [termToExpr t]
    -- General goal: call predicate
    _ -> termToExpr term

-- | Translate a Prolog term to an OrganIR expression.
termToExpr :: Term -> Expr
termToExpr = \case
    TmAtom a -> ECon (localName a) []
    TmVar "_" -> eVar "_"
    TmVar v -> eVar (T.toLower v)
    TmInt n -> eInt n
    TmFloat d -> eFloat d
    TmString s -> eString s
    TmCompound f args -> ECon (localName f) (map termToExpr args)
    TmList elts Nothing ->
        ECon (localName "[]") [EList (map termToExpr elts)]
    TmList elts (Just tail_) ->
        foldr (\e t -> ECon (localName "[|]") [termToExpr e, t]) (termToExpr tail_) elts
    TmOp op l r -> ECon (localName op) [termToExpr l, termToExpr r]
    TmParen t -> termToExpr t

-- | Collect all unique variables from clause heads for parameter names.
_clauseVars :: Term -> [Text]
_clauseVars t = nub (termVars t)
