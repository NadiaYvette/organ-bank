-- | Lexer for ISO Prolog.
module PrologFrontend.Lexer (Token (..), lexProlog) where

import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.Text (Text)
import Data.Text qualified as T

data Token
    = -- | Lowercase atom or quoted atom
      TAtom Text
    | -- | Variable (uppercase or _)
      TVar Text
    | TInt Integer
    | TFloat Double
    | -- | "double quoted"
      TString Text
    | TLParen
    | TRParen
    | TLBrack
    | TRBrack
    | -- | {
      TLBrace
    | -- | }
      TRBrace
    | TComma
    | -- | End-of-clause dot (followed by whitespace/eof)
      TDot
    | -- | |
      TBar
    | -- | Operator-like atom (symbolic chars)
      TOpAtom Text
    deriving (Eq, Show)

lexProlog :: Text -> Either String [Token]
lexProlog = go []
  where
    go acc t
        | T.null t = Right (reverse acc)
        | otherwise = case T.head t of
            c
                | isSpace c -> go acc (T.dropWhile isSpace t)
            '%' -> go acc (T.dropWhile (/= '\n') t)
            '/' ->
                let rest = T.tail t
                 in if not (T.null rest) && T.head rest == '*'
                        then skipBlockComment acc (T.tail rest)
                        else lexSymbolic acc t
            '(' -> go (TLParen : acc) (T.tail t)
            ')' -> go (TRParen : acc) (T.tail t)
            '[' -> go (TLBrack : acc) (T.tail t)
            ']' -> go (TRBrack : acc) (T.tail t)
            '{' -> go (TLBrace : acc) (T.tail t)
            '}' -> go (TRBrace : acc) (T.tail t)
            ',' -> go (TComma : acc) (T.tail t)
            '|' -> go (TBar : acc) (T.tail t)
            '!' -> go (TAtom "!" : acc) (T.tail t)
            ';' -> go (TAtom ";" : acc) (T.tail t)
            '\'' -> lexQuotedAtom acc (T.tail t)
            '"' -> lexString acc (T.tail t)
            '.' ->
                let rest = T.tail t
                 in if T.null rest || isSpace (T.head rest) || T.head rest == '%'
                        then go (TDot : acc) rest
                        else lexSymbolic acc t
            c
                | isLower c || c == '_' && T.length t == 1 -> lexWord acc t
                | isUpper c || c == '_' -> lexVar acc t
                | isDigit c -> lexNumber acc t
                | isSymbolic c -> lexSymbolic acc t
            c -> Left $ "Unexpected character: " ++ show c

    skipBlockComment acc t = case T.breakOn "*/" t of
        (_, rest)
            | T.null rest -> Left "Unterminated block comment"
            | otherwise -> go acc (T.drop 2 rest)

    lexQuotedAtom acc t = case T.break (\c -> c == '\'' || c == '\\') t of
        (s, rest)
            | T.null rest -> Left "Unterminated quoted atom"
            | T.head rest == '\'' ->
                let rest' = T.tail rest
                 in if not (T.null rest') && T.head rest' == '\''
                        then lexQuotedAtom' acc (s <> "'") (T.tail rest')
                        else go (TAtom s : acc) rest'
            | otherwise ->
                let rest' = T.tail rest
                 in if T.null rest'
                        then Left "Unterminated quoted atom escape"
                        else lexQuotedAtom' acc (s <> T.singleton (escChar (T.head rest'))) (T.tail rest')

    lexQuotedAtom' acc prefix t = case T.break (\c -> c == '\'' || c == '\\') t of
        (s, rest)
            | T.null rest -> Left "Unterminated quoted atom"
            | T.head rest == '\'' ->
                let rest' = T.tail rest
                 in if not (T.null rest') && T.head rest' == '\''
                        then lexQuotedAtom' acc (prefix <> s <> "'") (T.tail rest')
                        else go (TAtom (prefix <> s) : acc) rest'
            | otherwise ->
                let rest' = T.tail rest
                 in if T.null rest'
                        then Left "Unterminated quoted atom escape"
                        else lexQuotedAtom' acc (prefix <> s <> T.singleton (escChar (T.head rest'))) (T.tail rest')

    lexString acc t = case T.break (\c -> c == '"' || c == '\\') t of
        (s, rest)
            | T.null rest -> Left "Unterminated string"
            | T.head rest == '"' -> go (TString s : acc) (T.tail rest)
            | otherwise ->
                let rest' = T.tail rest
                 in if T.null rest'
                        then Left "Unterminated string escape"
                        else lexString' acc (T.snoc s (escChar (T.head rest'))) (T.tail rest')

    lexString' acc prefix t = case T.break (\c -> c == '"' || c == '\\') t of
        (s, rest)
            | T.null rest -> Left "Unterminated string"
            | T.head rest == '"' -> go (TString (prefix <> s) : acc) (T.tail rest)
            | otherwise ->
                let rest' = T.tail rest
                 in if T.null rest'
                        then Left "Unterminated string escape"
                        else lexString' acc (prefix <> s <> T.singleton (escChar (T.head rest'))) (T.tail rest')

    lexWord acc t =
        let (w, rest) = T.span isIdentChar t
         in go (TAtom w : acc) rest

    lexVar acc t =
        let (w, rest) = T.span isIdentChar t
         in go (TVar w : acc) rest

    lexNumber acc t =
        let (digits, rest) = T.span isDigit t
         in if not (T.null rest) && T.head rest == '.' && not (T.null (T.tail rest)) && isDigit (T.index rest 1)
                then
                    let (frac, rest') = T.span isDigit (T.tail rest)
                        numStr = T.unpack (digits <> "." <> frac)
                     in case reads numStr of
                            [(d, "")] -> go (TFloat d : acc) rest'
                            _ -> Left $ "Bad float: " ++ numStr
                else case reads (T.unpack digits) of
                    [(n, "")] -> go (TInt n : acc) rest
                    _ -> Left $ "Bad integer: " ++ T.unpack digits

    lexSymbolic acc t =
        let (sym, rest) = T.span isSymbolic t
         in go (TOpAtom sym : acc) rest

escChar :: Char -> Char
escChar = \case 'n' -> '\n'; 't' -> '\t'; '\\' -> '\\'; '\'' -> '\''; '"' -> '"'; c -> c

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

isSymbolic :: Char -> Bool
isSymbolic c = c `elem` ("+-*/\\^<>=~:.?@#&" :: String)
