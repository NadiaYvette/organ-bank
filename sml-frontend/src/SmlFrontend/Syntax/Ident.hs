-- | SML identifiers following the Definition §2.4.
module SmlFrontend.Syntax.Ident (
    VId (..),
    TyCon (..),
    TyVar (..),
    StrId (..),
    Lab (..),
    LongVId (..),
    LongTyCon (..),
    LongStrId (..),
    isEqualityTyVar,
)
where

import Data.Text (Text)

-- | Value identifier.
newtype VId = VId Text deriving (Eq, Ord, Show)

-- | Type constructor name.
newtype TyCon = TyCon Text deriving (Eq, Ord, Show)

-- | Type variable ('a, ''a).
data TyVar = TyVar
    { tvName :: Text
    , tvEquality :: Bool
    -- ^ True for ''a (equality type variable)
    }
    deriving (Eq, Ord, Show)

isEqualityTyVar :: TyVar -> Bool
isEqualityTyVar = tvEquality

-- | Structure identifier.
newtype StrId = StrId Text deriving (Eq, Ord, Show)

-- | Record label (field name or tuple index).
newtype Lab = Lab Text deriving (Eq, Ord, Show)

-- | Long identifiers (qualified with structure path).
data LongVId = LongVId [StrId] VId deriving (Eq, Ord, Show)

data LongTyCon = LongTyCon [StrId] TyCon deriving (Eq, Ord, Show)

data LongStrId = LongStrId [StrId] StrId deriving (Eq, Ord, Show)
