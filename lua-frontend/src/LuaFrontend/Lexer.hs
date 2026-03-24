-- | Lexer for Lua 5.4 source.
module LuaFrontend.Lexer (Token (..), lexLua) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T

data Token
    = TName Text
    | TInt Integer
    | TFloat Double
    | TString Text
    | -- | Keywords
      TKW Text
    | TLParen
    | TRParen
    | TLBrack
    | TRBrack
    | TLBrace
    | TRBrace
    | TComma
    | TSemicolon
    | TDot
    | -- | ..
      TDotDot
    | -- | ...
      TDotDotDot
    | TColon
    | -- | ::
      TColonColon
    | -- | #
      THash
    | -- | =
      TEq
    | -- | Operators
      TOp Text
    deriving (Eq, Show)

keywords :: [Text]
keywords =
    [ "and"
    , "break"
    , "do"
    , "else"
    , "elseif"
    , "end"
    , "false"
    , "for"
    , "function"
    , "goto"
    , "if"
    , "in"
    , "local"
    , "nil"
    , "not"
    , "or"
    , "repeat"
    , "return"
    , "then"
    , "true"
    , "until"
    , "while"
    ]

lexLua :: Text -> Either String [Token]
lexLua = go []
  where
    go acc t
        | T.null t = Right (reverse acc)
        | otherwise = case T.head t of
            c | isSpace c -> go acc (T.dropWhile isSpace t)
            '-' ->
                let rest = T.tail t
                 in if not (T.null rest) && T.head rest == '-'
                        then skipComment acc (T.tail rest)
                        else lexOp acc t
            '\'' -> lexShortString acc '\'' (T.tail t)
            '"' -> lexShortString acc '"' (T.tail t)
            '[' ->
                let rest = T.tail t
                 in if not (T.null rest) && (T.head rest == '[' || T.head rest == '=')
                        then lexLongStringTok acc rest
                        else go (TLBrack : acc) rest
            '(' -> go (TLParen : acc) (T.tail t)
            ')' -> go (TRParen : acc) (T.tail t)
            ']' -> go (TRBrack : acc) (T.tail t)
            '{' -> go (TLBrace : acc) (T.tail t)
            '}' -> go (TRBrace : acc) (T.tail t)
            ',' -> go (TComma : acc) (T.tail t)
            ';' -> go (TSemicolon : acc) (T.tail t)
            '#' -> go (THash : acc) (T.tail t)
            '.' -> lexDots acc (T.tail t)
            ':' ->
                let rest = T.tail t
                 in if not (T.null rest) && T.head rest == ':'
                        then go (TColonColon : acc) (T.tail rest)
                        else go (TColon : acc) rest
            '=' ->
                let rest = T.tail t
                 in if not (T.null rest) && T.head rest == '='
                        then go (TOp "==" : acc) (T.tail rest)
                        else go (TEq : acc) rest
            '~' ->
                let rest = T.tail t
                 in if not (T.null rest) && T.head rest == '='
                        then go (TOp "~=" : acc) (T.tail rest)
                        else go (TOp "~" : acc) rest
            '<' -> lexCompound acc '<' ["<=", "<<"] (T.tail t)
            '>' -> lexCompound acc '>' [">=", ">>"] (T.tail t)
            c | isAlpha c || c == '_' -> lexName acc t
            c | isDigit c -> lexNumber acc t
            c | isOpChar c -> lexOp acc t
            c -> Left $ "Unexpected character: " ++ show c

    skipComment acc t
        | T.null t = Right (reverse acc)
        | T.head t == '[' = skipLongComment acc (T.tail t)
        | otherwise = go acc (T.dropWhile (/= '\n') t)

    skipLongComment acc t =
        let (eqs, rest) = T.span (== '=') t
            level = T.length eqs
         in if not (T.null rest) && T.head rest == '['
                then skipLongBody acc level (T.tail rest)
                else go acc (T.dropWhile (/= '\n') (eqs <> rest))

    skipLongBody acc level t = case T.breakOn "]" t of
        (_, rest)
            | T.null rest -> Left "Unterminated long comment"
            | otherwise ->
                let rest' = T.tail rest
                    (eqs, rest'') = T.span (== '=') rest'
                 in if T.length eqs == level && not (T.null rest'') && T.head rest'' == ']'
                        then go acc (T.tail rest'')
                        else skipLongBody acc level rest'

    lexShortString acc delim = lexStr acc delim ""

    lexStr acc delim prefix t = case T.break (\c -> c == delim || c == '\\' || c == '\n') t of
        (s, rest)
            | T.null rest -> Left "Unterminated string"
            | T.head rest == '\n' -> Left "Unterminated string (newline)"
            | T.head rest == delim -> go (TString (prefix <> s) : acc) (T.tail rest)
            | otherwise ->
                let rest' = T.tail rest
                 in if T.null rest'
                        then Left "Unterminated string escape"
                        else lexStr acc delim (prefix <> s <> T.singleton (escChar (T.head rest'))) (T.tail rest')

    lexLongStringTok acc t =
        let (eqs, rest) = T.span (== '=') t
            level = T.length eqs
         in if not (T.null rest) && T.head rest == '['
                then lexLongBody acc level "" (T.tail rest)
                else go (TLBrack : acc) (eqs <> rest) -- not a long string, re-lex
    lexLongBody acc level prefix t = case T.breakOn "]" t of
        (s, rest)
            | T.null rest -> Left "Unterminated long string"
            | otherwise ->
                let rest' = T.tail rest
                    (eqs, rest'') = T.span (== '=') rest'
                 in if T.length eqs == level && not (T.null rest'') && T.head rest'' == ']'
                        then go (TString (prefix <> s) : acc) (T.tail rest'')
                        else lexLongBody acc level (prefix <> s <> "]") rest'

    lexDots acc t
        | T.null t = go (TDot : acc) t
        | T.head t == '.' =
            let rest = T.tail t
             in if not (T.null rest) && T.head rest == '.'
                    then go (TDotDotDot : acc) (T.tail rest)
                    else go (TDotDot : acc) rest
        | isDigit (T.head t) = lexFracNumber acc "0." t
        | otherwise = go (TDot : acc) t

    lexName acc t =
        let (w, rest) = T.span isIdentChar t
         in if w `elem` keywords
                then go (TKW w : acc) rest
                else go (TName w : acc) rest

    lexNumber acc t =
        let (digits, rest) = T.span isDigit t
         in if not (T.null rest) && T.head rest == '.' && not (T.null (T.tail rest)) && isDigit (T.index rest 1)
                then lexFracNumber acc (digits <> ".") (T.tail rest)
                else case reads (T.unpack digits) of
                    [(n, "")] -> go (TInt n : acc) rest
                    _ -> Left $ "Bad number: " ++ T.unpack digits

    lexFracNumber acc prefix t =
        let (frac, rest) = T.span isDigit t
            numStr = T.unpack (prefix <> frac)
         in case reads numStr of
                [(d, "")] -> go (TFloat d : acc) rest
                _ -> Left $ "Bad float: " ++ numStr

    lexOp acc t =
        let (op, rest) = T.span isOpChar t
         in go (TOp op : acc) rest

    lexCompound acc base pairs rest
        | not (T.null rest) =
            let two = T.pack [base, T.head rest]
             in if two `elem` pairs
                    then go (TOp two : acc) (T.tail rest)
                    else go (TOp (T.singleton base) : acc) rest
        | otherwise = go (TOp (T.singleton base) : acc) rest

escChar :: Char -> Char
escChar = \case
    'n' -> '\n'
    't' -> '\t'
    'r' -> '\r'
    '\\' -> '\\'
    '\'' -> '\''
    '"' -> '"'
    'a' -> '\a'
    'b' -> '\b'
    'f' -> '\f'
    'v' -> '\v'
    c -> c

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

isOpChar :: Char -> Bool
isOpChar c = c `elem` ("+-*/%^&|~<>=" :: String)
