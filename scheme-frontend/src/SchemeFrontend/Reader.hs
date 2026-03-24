-- | S-expression reader: tokens → datum tree.
module SchemeFrontend.Reader (Datum (..), readDatums) where

import Data.Text (Text)
import SchemeFrontend.Lexer (Token (..))

-- | S-expression datum.
data Datum
    = DList [Datum]
    | DDottedList [Datum] Datum
    | DVector [Datum]
    | DInt Integer
    | DFloat Double
    | DString Text
    | DChar Char
    | DBool Bool
    | DSymbol Text
    | DQuote Datum
    | DQuasiquote Datum
    | DUnquote Datum
    | DUnquoteSplicing Datum
    deriving (Eq, Show)

readDatums :: [Token] -> Either String [Datum]
readDatums = go []
  where
    go acc [] = Right (reverse acc)
    go acc (TEof : _) = Right (reverse acc)
    go acc toks = do
        (d, rest) <- readDatum toks
        go (d : acc) rest

readDatum :: [Token] -> Either String (Datum, [Token])
readDatum = \case
    [] -> Left "Unexpected end of input"
    TEof : _ -> Left "Unexpected end of input"
    TLParen : rest -> readListBody [] rest
    TLBrack : rest -> readBrackList [] rest
    THashLParen : rest -> readVector [] rest
    TQuote : rest -> wrap DQuote rest
    TQuasiquote : rest -> wrap DQuasiquote rest
    TUnquote : rest -> wrap DUnquote rest
    TUnquoteSplicing : rest -> wrap DUnquoteSplicing rest
    TInt n : rest -> Right (DInt n, rest)
    TFloat d : rest -> Right (DFloat d, rest)
    TString s : rest -> Right (DString s, rest)
    TChar c : rest -> Right (DChar c, rest)
    THashT : rest -> Right (DBool True, rest)
    THashF : rest -> Right (DBool False, rest)
    TIdent s : rest -> Right (DSymbol s, rest)
    t : _ -> Left $ "Unexpected token: " ++ show t
  where
    wrap f toks = do
        (d, rest) <- readDatum toks
        Right (f d, rest)

readListBody :: [Datum] -> [Token] -> Either String (Datum, [Token])
readListBody acc = \case
    TRParen : rest -> Right (DList (reverse acc), rest)
    TDot : rest -> do
        (d, rest') <- readDatum rest
        case rest' of
            TRParen : rest'' -> Right (DDottedList (reverse acc) d, rest'')
            _ -> Left "Expected ) after dotted pair tail"
    [] -> Left "Unterminated list"
    toks -> do
        (d, rest) <- readDatum toks
        readListBody (d : acc) rest

readBrackList :: [Datum] -> [Token] -> Either String (Datum, [Token])
readBrackList acc = \case
    TRBrack : rest -> Right (DList (reverse acc), rest)
    [] -> Left "Unterminated bracketed list"
    toks -> do
        (d, rest) <- readDatum toks
        readBrackList (d : acc) rest

readVector :: [Datum] -> [Token] -> Either String (Datum, [Token])
readVector acc = \case
    TRParen : rest -> Right (DVector (reverse acc), rest)
    [] -> Left "Unterminated vector"
    toks -> do
        (d, rest) <- readDatum toks
        readVector (d : acc) rest
