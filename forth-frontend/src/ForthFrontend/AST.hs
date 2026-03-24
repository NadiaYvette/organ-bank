-- | Forth AST representation.
module ForthFrontend.AST where

import Data.Text (Text)

-- | A top-level Forth item.
data ForthItem
    = -- | A colon definition: @: name body ;@
      FDef Text [ForthItem]
    | -- | @VARIABLE name@
      FVariable Text
    | -- | @CONSTANT name@ (value comes from preceding expression on stack)
      FConstant Text
    | -- | @IF body [ELSE body] THEN@
      FIf [ForthItem] (Maybe [ForthItem])
    | -- | @BEGIN body UNTIL@
      FBeginUntil [ForthItem]
    | -- | @BEGIN body WHILE body REPEAT@
      FBeginWhile [ForthItem] [ForthItem]
    | -- | @DO body LOOP@ or @DO body +LOOP@
      FDoLoop [ForthItem]
    | -- | A literal integer
      FLitInt Integer
    | -- | A literal float
      FLitFloat Double
    | -- | A string literal
      FLitString Text
    | -- | A word reference (built-in or user-defined)
      FWord Text
    deriving (Show)
