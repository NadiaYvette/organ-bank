-- | Core Scheme AST after desugaring.
module SchemeFrontend.AST where

import Data.Text (Text)

-- | A top-level form.
data TopLevel
    = TLDefine Text Core
    | TLDefineRec [(Text, Core)]
    | TLExpr Core
    deriving (Show)

-- | Core expression — the desugared target.
data Core
    = CVar Text
    | CLitInt Integer
    | CLitFloat Double
    | CLitString Text
    | CLitChar Char
    | CLitBool Bool
    | CLam [Text] Core
    | CApp Core [Core]
    | CIf Core Core Core
    | CLet [(Text, Core)] Core
    | CLetRec [(Text, Core)] Core
    | CSet Text Core
    | CBegin [Core]
    | CQuote QuoteVal
    | CVoid
    deriving (Show)

-- | Quoted values.
data QuoteVal
    = QSymbol Text
    | QInt Integer
    | QFloat Double
    | QString Text
    | QChar Char
    | QBool Bool
    | QList [QuoteVal]
    | QDottedList [QuoteVal] QuoteVal
    | QVector [QuoteVal]
    deriving (Show)
