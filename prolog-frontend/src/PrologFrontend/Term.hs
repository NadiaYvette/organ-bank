-- | Prolog term representation and top-level clause types.
module PrologFrontend.Term where

import Data.Text (Text)

-- | A Prolog term.
data Term
    = -- | Atom (including operators as atoms)
      TmAtom Text
    | -- | Variable
      TmVar Text
    | TmInt Integer
    | TmFloat Double
    | -- | Double-quoted string (atom or char list)
      TmString Text
    | -- | f(X, Y, ...)
      TmCompound Text [Term]
    | -- | [H1, H2 | Tail]  or  [a, b, c]
      TmList [Term] (Maybe Term)
    | -- | Infix operator (desugared to TmCompound)
      TmOp Text Term Term
    | -- | Parenthesised (dropped after parsing)
      TmParen Term
    deriving (Eq, Show)

-- | A top-level sentence in a Prolog source file.
data Sentence
    = -- | head :- body  or  head (fact)
      SClause Term (Maybe Term)
    | -- | :- goal
      SDirective Term
    | -- | ?- goal
      SQuery Term
    deriving (Eq, Show)

-- | Functor name + arity key for grouping clauses.
type PredKey = (Text, Int)

-- | Get the functor/arity of a head term.
predKey :: Term -> Maybe PredKey
predKey = \case
    TmAtom a -> Just (a, 0)
    TmCompound f args -> Just (f, length args)
    _ -> Nothing

-- | Flatten a conjunction (a, b, c) into a list of goals.
flattenConj :: Term -> [Term]
flattenConj = \case
    TmCompound "," [l, r] -> flattenConj l ++ flattenConj r
    TmOp "," l r -> flattenConj l ++ flattenConj r
    t -> [t]

-- | Flatten a disjunction (a ; b ; c) into a list of alternatives.
flattenDisj :: Term -> [Term]
flattenDisj = \case
    TmCompound ";" [l, r] -> flattenDisj l ++ flattenDisj r
    TmOp ";" l r -> flattenDisj l ++ flattenDisj r
    t -> [t]

-- | Collect all variable names in a term.
termVars :: Term -> [Text]
termVars = \case
    TmVar v | v /= "_" -> [v]
    TmCompound _ args -> concatMap termVars args
    TmList elts tail_ -> concatMap termVars elts ++ maybe [] termVars tail_
    TmOp _ l r -> termVars l ++ termVars r
    TmParen t -> termVars t
    _ -> []
