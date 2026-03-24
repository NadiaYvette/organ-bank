-- | Lua AST representation (Lua 5.4 subset).
module LuaFrontend.AST where

import Data.Text (Text)

-- | A block is a sequence of statements.
type Block = [Stat]

-- | Statements.
data Stat
    = SAssign [LVar] [Expr]
    | SLocal [Text] [Expr]
    | -- | local function name(params...) body end
      SLocalFunc Text [Text] Bool Block
    | SDo Block
    | SWhile Expr Block
    | SRepeat Block Expr
    | -- | if/elseif.../else/end
      SIf Expr Block [(Expr, Block)] (Maybe Block)
    | -- | for i=start,stop[,step] do body end
      SForNum Text Expr Expr (Maybe Expr) Block
    | -- | for names in exprs do body end
      SForIn [Text] [Expr] Block
    | SReturn [Expr]
    | SBreak
    | -- | function name(params...) body end
      SFunc FuncName [Text] Bool Block
    | -- | function call as statement
      SExprStat Expr
    deriving (Show)

-- | Function name: a.b.c or a.b.c:d
data FuncName = FuncName [Text] (Maybe Text)
    deriving (Show)

-- | L-values (assignable variables).
data LVar
    = LVName Text
    | -- | table[key]
      LVIndex Expr Expr
    | -- | table.field
      LVField Expr Text
    deriving (Show)

-- | Expressions.
data Expr
    = ENil
    | ETrue
    | EFalse
    | EInt Integer
    | EFloat Double
    | EString Text
    | -- | ...
      EVarArg
    | EName Text
    | -- | expr[expr]
      EIndex Expr Expr
    | -- | expr.name
      EField Expr Text
    | -- | expr:name(args)
      EMethodCall Expr Text [Expr]
    | -- | expr(args)
      ECall Expr [Expr]
    | EBinOp Text Expr Expr
    | EUnOp Text Expr
    | -- | function(params...) body end
      EFunc [Text] Bool Block
    | -- | { fields }
      ETable [Field]
    deriving (Show)

-- | Table constructor fields.
data Field
    = -- | [expr] = expr
      FKey Expr Expr
    | -- | name = expr
      FName Text Expr
    | -- | expr (positional)
      FExpr Expr
    deriving (Show)
