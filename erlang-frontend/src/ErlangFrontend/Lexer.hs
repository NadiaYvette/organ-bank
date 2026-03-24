-- | Lexer for Erlang source.
module ErlangFrontend.Lexer (Token (..), lexErlang) where

import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.Text (Text)
import Data.Text qualified as T

data Token
    = TAtom Text
    | TVar Text
    | TInt Integer
    | TFloat Double
    | TString Text
    | TChar Char
    | TLParen
    | TRParen
    | TLBrack
    | TRBrack
    | TLBrace
    | TRBrace
    | TComma
    | -- | End-of-form dot (followed by whitespace/eof)
      TDot
    | TSemicolon
    | TBar
    | -- | ||
      TBarBar
    | -- | ->
      TArrow
    | -- | #
      THashSign
    | -- | ::
      TColonColon
    | -- | Keywords: when, if, case, of, end, fun, receive, after, begin, catch, try, etc.
      TKW Text
    | -- | Operators
      TOp Text
    deriving (Eq, Show)

keywords :: [Text]
keywords =
    [ "after"
    , "begin"
    , "case"
    , "catch"
    , "end"
    , "fun"
    , "if"
    , "of"
    , "receive"
    , "try"
    , "when"
    , "andalso"
    , "orelse"
    , "not"
    , "and"
    , "or"
    , "xor"
    , "band"
    , "bor"
    , "bxor"
    , "bnot"
    , "bsl"
    , "bsr"
    , "div"
    , "rem"
    ]

lexErlang :: Text -> Either String [Token]
lexErlang = go []
  where
    go acc t
        | T.null t = Right (reverse acc)
        | otherwise = case T.head t of
            c
                | isSpace c -> go acc (T.dropWhile isSpace t)
            '%' -> go acc (T.dropWhile (/= '\n') t)
            '(' -> go (TLParen : acc) (T.tail t)
            ')' -> go (TRParen : acc) (T.tail t)
            '[' -> go (TLBrack : acc) (T.tail t)
            ']' -> go (TRBrack : acc) (T.tail t)
            '{' -> go (TLBrace : acc) (T.tail t)
            '}' -> go (TRBrace : acc) (T.tail t)
            ',' -> go (TComma : acc) (T.tail t)
            ';' -> go (TSemicolon : acc) (T.tail t)
            '#' -> go (THashSign : acc) (T.tail t)
            '|' ->
                let rest = T.tail t
                 in if not (T.null rest) && T.head rest == '|'
                        then go (TBarBar : acc) (T.tail rest)
                        else go (TBar : acc) rest
            '.' ->
                let rest = T.tail t
                 in if T.null rest || isSpace (T.head rest) || T.head rest == '%'
                        then go (TDot : acc) rest
                        else go (TOp "." : acc) rest
            '\'' -> lexQuotedAtom acc (T.tail t)
            '"' -> lexString acc (T.tail t)
            '$' ->
                let rest = T.tail t
                 in if T.null rest
                        then Left "Unexpected end of input after $"
                        else
                            if T.head rest == '\\'
                                then
                                    let rest' = T.tail rest
                                     in if T.null rest'
                                            then Left "Unexpected end of input after $\\"
                                            else go (TChar (escChar (T.head rest')) : acc) (T.tail rest')
                                else go (TChar (T.head rest) : acc) (T.tail rest)
            '-' ->
                let rest = T.tail t
                 in if not (T.null rest) && T.head rest == '>'
                        then go (TArrow : acc) (T.tail rest)
                        else
                            if not (T.null rest) && isDigit (T.head rest)
                                then lexNumber acc t
                                else go (TOp "-" : acc) rest
            ':' ->
                let rest = T.tail t
                 in if not (T.null rest) && T.head rest == ':'
                        then go (TColonColon : acc) (T.tail rest)
                        else go (TOp ":" : acc) rest
            c
                | isLower c -> lexWord acc t
                | c == '_' || isUpper c -> lexVarToken acc t
                | isDigit c -> lexNumber acc t
                | isOpChar c -> lexOp acc t
            c -> Left $ "Unexpected character: " ++ show c

    lexQuotedAtom acc t = case T.break (\c -> c == '\'' || c == '\\') t of
        (s, rest)
            | T.null rest -> Left "Unterminated quoted atom"
            | T.head rest == '\'' -> go (TAtom s : acc) (T.tail rest)
            | otherwise ->
                let rest' = T.tail rest
                 in if T.null rest'
                        then Left "Unterminated atom escape"
                        else lexQuotedAtom' acc (T.snoc s (escChar (T.head rest'))) (T.tail rest')

    lexQuotedAtom' acc prefix t = case T.break (\c -> c == '\'' || c == '\\') t of
        (s, rest)
            | T.null rest -> Left "Unterminated quoted atom"
            | T.head rest == '\'' -> go (TAtom (prefix <> s) : acc) (T.tail rest)
            | otherwise ->
                let rest' = T.tail rest
                 in if T.null rest'
                        then Left "Unterminated atom escape"
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
         in if w `elem` keywords
                then go (TKW w : acc) rest
                else go (TAtom w : acc) rest

    lexVarToken acc t =
        let (w, rest) = T.span isIdentChar t
         in go (TVar w : acc) rest

    lexNumber acc t =
        let (sign, t') = if T.head t == '-' then ("-", T.tail t) else ("", t)
            (digits, rest) = T.span isDigit t'
         in if not (T.null rest) && T.head rest == '.' && not (T.null (T.tail rest)) && isDigit (T.index rest 1)
                then
                    let (frac, rest') = T.span isDigit (T.tail rest)
                        numStr = T.unpack (sign <> digits <> "." <> frac)
                     in case reads numStr of
                            [(d, "")] -> go (TFloat d : acc) rest'
                            _ -> Left $ "Bad float: " ++ numStr
                else case reads (T.unpack (sign <> digits)) of
                    [(n, "")] -> go (TInt n : acc) rest
                    _ -> Left $ "Bad integer: " ++ T.unpack (sign <> digits)

    lexOp acc t =
        let (op, rest) = T.span isOpChar t
         in go (TOp op : acc) rest

escChar :: Char -> Char
escChar = \case 'n' -> '\n'; 't' -> '\t'; 'r' -> '\r'; '\\' -> '\\'; '\'' -> '\''; '"' -> '"'; c -> c

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '@'

isOpChar :: Char -> Bool
isOpChar c = c `elem` ("+-*/=<>!:." :: String)
