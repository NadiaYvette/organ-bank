-- | Lexer for R7RS-small Scheme.
module SchemeFrontend.Lexer (Token (..), lexScheme) where

import Data.Char (isAlpha, isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T

data Token
    = TLParen
    | TRParen
    | TLBrack
    | TRBrack
    | TQuote
    | TQuasiquote
    | TUnquote
    | TUnquoteSplicing
    | TDot
    | THashT
    | THashF
    | THashLParen -- #(
    | TInt Integer
    | TFloat Double
    | TString Text
    | TChar Char
    | TIdent Text
    | TEof
    deriving (Eq, Show)

lexScheme :: Text -> Either String [Token]
lexScheme = go []
  where
    go acc t
        | T.null t = Right (reverse acc)
        | otherwise = case T.head t of
            c
                | isSpace c -> go acc (T.dropWhile isSpace t)
            ';' -> go acc (T.dropWhile (/= '\n') t)
            '(' -> go (TLParen : acc) (T.tail t)
            ')' -> go (TRParen : acc) (T.tail t)
            '[' -> go (TLBrack : acc) (T.tail t)
            ']' -> go (TRBrack : acc) (T.tail t)
            '\'' -> go (TQuote : acc) (T.tail t)
            '`' -> go (TQuasiquote : acc) (T.tail t)
            ',' ->
                let rest = T.tail t
                 in if not (T.null rest) && T.head rest == '@'
                        then go (TUnquoteSplicing : acc) (T.tail rest)
                        else go (TUnquote : acc) rest
            '"' -> lexString acc (T.tail t)
            '#' -> lexHash acc (T.tail t)
            c
                | isDigit c || (c == '-' && not (T.null (T.tail t)) && isDigit (T.index t 1)) ->
                    lexNumber acc t
                | isIdentStart c -> lexIdent acc t
                | c == '.' ->
                    let rest = T.tail t
                     in if T.null rest || not (isIdentChar (T.head rest))
                            then go (TDot : acc) rest
                            else lexIdent acc t
            c -> Left $ "Unexpected character: " ++ [c]

    lexString acc t = case T.break (\c -> c == '"' || c == '\\') t of
        (s, rest)
            | T.null rest -> Left "Unterminated string"
            | T.head rest == '"' -> go (TString s : acc) (T.tail rest)
            | otherwise ->
                -- backslash escape
                let rest' = T.tail rest
                 in if T.null rest'
                        then Left "Unterminated string escape"
                        else case T.head rest' of
                            'n' -> lexString' acc (T.snoc s '\n') (T.tail rest')
                            't' -> lexString' acc (T.snoc s '\t') (T.tail rest')
                            '\\' -> lexString' acc (T.snoc s '\\') (T.tail rest')
                            '"' -> lexString' acc (T.snoc s '"') (T.tail rest')
                            c -> Left $ "Unknown escape: \\" ++ [c]

    lexString' acc prefix t = case T.break (\c -> c == '"' || c == '\\') t of
        (s, rest)
            | T.null rest -> Left "Unterminated string"
            | T.head rest == '"' -> go (TString (prefix <> s) : acc) (T.tail rest)
            | otherwise ->
                let rest' = T.tail rest
                 in if T.null rest'
                        then Left "Unterminated string escape"
                        else case T.head rest' of
                            'n' -> lexString' acc (prefix <> s <> "\n") (T.tail rest')
                            't' -> lexString' acc (prefix <> s <> "\t") (T.tail rest')
                            '\\' -> lexString' acc (prefix <> s <> "\\") (T.tail rest')
                            '"' -> lexString' acc (prefix <> s <> "\"") (T.tail rest')
                            c -> Left $ "Unknown escape: \\" ++ [c]

    lexHash acc t
        | T.null t = Left "Unexpected end of input after #"
        | otherwise = case T.head t of
            't' -> go (THashT : acc) (T.tail t)
            'f' -> go (THashF : acc) (T.tail t)
            '(' -> go (THashLParen : acc) (T.tail t)
            '\\' -> lexCharLit acc (T.tail t)
            _ -> Left $ "Unknown # syntax: #" ++ [T.head t]

    lexCharLit acc t
        | T.null t = Left "Unexpected end of input after #\\"
        | otherwise =
            let (name, rest) = T.span isAlpha t
             in if T.length name > 1
                    then case name of
                        "space" -> go (TChar ' ' : acc) rest
                        "newline" -> go (TChar '\n' : acc) rest
                        "tab" -> go (TChar '\t' : acc) rest
                        _ -> Left $ "Unknown character name: " ++ T.unpack name
                    else go (TChar (T.head t) : acc) (T.tail t)

    lexNumber acc t =
        let (numStr, rest) = T.span (\c -> isDigit c || c == '.' || c == '-' || c == 'e' || c == 'E') t
         in if T.any (== '.') numStr || T.any (\c -> c == 'e' || c == 'E') numStr
                then case reads (T.unpack numStr) of
                    [(d, "")] -> go (TFloat d : acc) rest
                    _ -> Left $ "Bad number: " ++ T.unpack numStr
                else case reads (T.unpack numStr) of
                    [(n, "")] -> go (TInt n : acc) rest
                    _ -> Left $ "Bad number: " ++ T.unpack numStr

    lexIdent acc t =
        let (ident, rest) = T.span isIdentChar t
         in go (TIdent (T.toLower ident) : acc) rest

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c `elem` ("!$%&*/:<=>?^_~+-" :: String)

isIdentChar :: Char -> Bool
isIdentChar c = isIdentStart c || isDigit c || c `elem` (".@" :: String)
