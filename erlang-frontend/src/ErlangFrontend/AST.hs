-- | Erlang AST representation.
module ErlangFrontend.AST where

import Data.Text (Text)

-- | A top-level form in an Erlang source file.
data Form
    = FModule Text
    | FExport [(Text, Int)]
    | FFun Text [FunClause]
    | FAttribute Text [Expr]
    deriving (Show)

-- | A function clause: patterns, optional guard, body.
data FunClause = FunClause
    { fcPats :: [Pat]
    , fcGuard :: Maybe GuardSeq
    , fcBody :: [Expr]
    }
    deriving (Show)

{- | Guard sequence: list of guard tests (semicolons = or, commas = and).
Outer list = disjunction (;), inner list = conjunction (,).
-}
type GuardSeq = [[Expr]]

-- | Expressions.
data Expr
    = EAtom Text
    | EVar Text
    | EInt Integer
    | EFloat Double
    | EString Text
    | EChar Char
    | ETuple [Expr]
    | -- | [H1, H2 | Tail]
      EList [Expr] (Maybe Expr)
    | EBinOp Text Expr Expr
    | EUnOp Text Expr
    | -- | f(A1, A2) or Mod:Fun(Args)
      ECall Expr [Expr]
    | -- | Mod:Fun
      ERemote Expr Expr
    | -- | fun name/arity
      EFun Text Int
    | -- | fun Mod:Fun/Arity
      EFunRef Expr Expr Int
    | ELambda [FunClause]
    | ECase Expr [CaseClause]
    | EIf [CaseClause]
    | EReceive [CaseClause] (Maybe (Expr, [Expr]))
    | EBegin [Expr]
    | ETry [Expr] [CaseClause] [CaseClause] [Expr]
    | ECatch Expr
    | EListComp Expr [Qualifier]
    | -- | Pat = Expr
      EMatch Expr Expr
    | -- | Pid ! Msg
      ESend Expr Expr
    deriving (Show)

-- | Case clause: pattern, optional guard, body.
data CaseClause = CaseClause
    { ccPat :: Pat
    , ccGuard :: Maybe GuardSeq
    , ccBody :: [Expr]
    }
    deriving (Show)

-- | Patterns (structurally same as expressions for Erlang).
data Pat
    = PAtom Text
    | PVar Text
    | PWild
    | PInt Integer
    | PFloat Double
    | PString Text
    | PChar Char
    | PTuple [Pat]
    | PList [Pat] (Maybe Pat)
    | PCons Pat Pat
    | -- | Pat1 = Pat2
      PMatch Pat Pat
    | PBinOp Text Pat Pat
    deriving (Show)

-- | List comprehension qualifiers.
data Qualifier
    = -- | Pat <- Expr
      QGenerator Pat Expr
    | QFilter Expr
    deriving (Show)
