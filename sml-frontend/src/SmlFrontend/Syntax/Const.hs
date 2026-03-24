-- | SML special constants (Definition §2.2).
module SmlFrontend.Syntax.Const (SCon (..)) where

import Data.Text (Text)

-- | Special constants.
data SCon
    = -- | Integer literal
      SInt Integer
    | -- | Word literal (0wxFF)
      SWord Integer
    | -- | Real literal
      SReal Double
    | -- | String literal
      SString Text
    | -- | Character literal (#"c")
      SChar Char
    deriving (Eq, Show)
