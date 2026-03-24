-- | Lexer for Forth source.
module ForthFrontend.Lexer (Token (..), lexForth) where

import Data.Char (isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T

data Token
    = -- | Any word (includes operators, builtins, user words)
      TWord Text
    | TInt Integer
    | TFloat Double
    | -- | String from ." ..." or s" ..."
      TString Text
    | -- | : (start definition)
      TColon
    | -- | ; (end definition)
      TSemicolon
    deriving (Eq, Show)

lexForth :: Text -> Either String [Token]
lexForth = go []
  where
    go acc t
        | T.null t = Right (reverse acc)
        | otherwise =
            let t' = T.dropWhile isSpace t
             in if T.null t'
                    then Right (reverse acc)
                    else lexOne acc t'

    lexOne acc t = case T.head t of
        '\\' -> go acc (T.dropWhile (/= '\n') t) -- line comment
        '(' -> skipParenComment acc (T.tail t) -- ( comment )
        ':' ->
            let rest = T.tail t
             in if T.null rest || isSpace (T.head rest)
                    then go (TColon : acc) rest
                    else lexWord acc t
        ';' -> go (TSemicolon : acc) (T.tail t)
        '"' -> go acc (T.tail t) -- stray quote, skip
        _ -> lexWordOrNum acc t

    skipParenComment acc t = case T.break (== ')') t of
        (_, rest)
            | T.null rest -> Left "Unterminated ( comment"
            | otherwise -> go acc (T.tail rest)

    lexWordOrNum acc t =
        let (w, rest) = T.break isSpace t
         in case w of
                _ | isDotQuote w -> lexDotString acc rest
                _ | isSQuote w -> lexSString acc rest
                _ | isNumber w -> go (parseNumber w : acc) rest
                _ -> go (TWord w : acc) rest

    isDotQuote w = w == ".\"" || w == ".\""
    isSQuote w = T.toLower w == "s\""

    lexDotString acc t =
        let (s, rest) = T.break (== '"') t
         in if T.null rest
                then Left "Unterminated .\" string"
                else go (TString (T.strip s) : acc) (T.tail rest)

    lexSString acc t =
        let t' = if not (T.null t) && T.head t == ' ' then T.tail t else t
            (s, rest) = T.break (== '"') t'
         in if T.null rest
                then Left "Unterminated s\" string"
                else go (TString s : acc) (T.tail rest)

    isNumber w =
        let w' = if T.head w == '-' && T.length w > 1 then T.tail w else w
         in T.all (\c -> isDigit c || c == '.') w' && not (T.null w')

    parseNumber w
        | T.any (== '.') w = case reads (T.unpack w) of
            [(d, "")] -> TFloat d
            _ -> TWord w
        | otherwise = case reads (T.unpack w) of
            [(n, "")] -> TInt n
            _ -> TWord w

    lexWord acc t =
        let (w, rest) = T.break isSpace t
         in go (TWord w : acc) rest
