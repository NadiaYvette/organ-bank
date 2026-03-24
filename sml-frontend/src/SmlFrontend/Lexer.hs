-- | Hand-written lexer for Standard ML (Definition §2.1-2.5).
module SmlFrontend.Lexer (
    Token (..),
    TokenKind (..),
    lexSml,
)
where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import SmlFrontend.Syntax.AST (Pos (..))

data Token = Token {tokKind :: !TokenKind, tokPos :: !Pos}
    deriving (Show)

data TokenKind
    = -- Keywords
      KAbstype
    | KAnd
    | KAndalso
    | KAs
    | KCase
    | KDatatype
    | KDo
    | KElse
    | KEnd
    | KException
    | KFn
    | KFun
    | KHandle
    | KIf
    | KIn
    | KInfix
    | KInfixr
    | KLet
    | KLocal
    | KNonfix
    | KOf
    | KOp
    | KOpen
    | KOrelse
    | KRaise
    | KRec
    | KThen
    | KType
    | KVal
    | KWith
    | KWhile
    | -- Delimiters
      KLParen
    | KRParen
    | KLBrack
    | KRBrack
    | KLBrace
    | KRBrace
    | KComma
    | KSemicolon
    | KDots
    | KDot
    | -- Special
      KArrow
    | KDArrow
    | KColon
    | KBar
    | KEquals
    | KHash
    | KUnderscore
    | KStar
    | -- Literals
      KInt Integer
    | KWord Integer
    | KReal Double
    | KString Text
    | KChar Char
    | -- Identifiers
      KId Text
    | KSymId Text
    | KTyVar Text Bool
    | KEOF
    deriving (Eq, Show)

-- | Lexer state.
data LState = LState !String !Int !Int

type LResult a = Either String a

reserved :: [(Text, TokenKind)]
reserved =
    [ ("abstype", KAbstype)
    , ("and", KAnd)
    , ("andalso", KAndalso)
    , ("as", KAs)
    , ("case", KCase)
    , ("datatype", KDatatype)
    , ("do", KDo)
    , ("else", KElse)
    , ("end", KEnd)
    , ("exception", KException)
    , ("fn", KFn)
    , ("fun", KFun)
    , ("handle", KHandle)
    , ("if", KIf)
    , ("in", KIn)
    , ("infix", KInfix)
    , ("infixr", KInfixr)
    , ("let", KLet)
    , ("local", KLocal)
    , ("nonfix", KNonfix)
    , ("of", KOf)
    , ("op", KOp)
    , ("open", KOpen)
    , ("orelse", KOrelse)
    , ("raise", KRaise)
    , ("rec", KRec)
    , ("then", KThen)
    , ("type", KType)
    , ("val", KVal)
    , ("with", KWith)
    , ("while", KWhile)
    ]

isSymChar :: Char -> Bool
isSymChar c = c `elem` ("!%&$#+-/:<=>?@\\~`^|*" :: String)

isIdTail :: Char -> Bool
isIdTail c = isAlphaNum c || c == '_' || c == '\''

lexSml :: Text -> Either String [Token]
lexSml input = lexAll (LState (T.unpack input) 1 1)

lexAll :: LState -> LResult [Token]
lexAll st = do
    (tok, st') <- lexOne st
    case tokKind tok of
        KEOF -> return [tok]
        _ -> do
            rest <- lexAll st'
            return (tok : rest)

lexOne :: LState -> LResult (Token, LState)
lexOne (LState [] ln col) = Right (Token KEOF (Pos ln col), LState [] ln col)
lexOne (LState ('\n' : cs) ln _col) = lexOne (LState cs (ln + 1) 1)
lexOne (LState (c : cs) ln col) | isSpace c = lexOne (LState cs ln (col + 1))
-- Nested comments
lexOne (LState ('(' : '*' : cs) ln col) = skipComment (LState cs ln (col + 2)) 1
-- Delimiters
lexOne (LState ('(' : cs) ln col) = emit KLParen cs ln col 1
lexOne (LState (')' : cs) ln col) = emit KRParen cs ln col 1
lexOne (LState ('[' : cs) ln col) = emit KLBrack cs ln col 1
lexOne (LState (']' : cs) ln col) = emit KRBrack cs ln col 1
lexOne (LState ('{' : cs) ln col) = emit KLBrace cs ln col 1
lexOne (LState ('}' : cs) ln col) = emit KRBrace cs ln col 1
lexOne (LState (',' : cs) ln col) = emit KComma cs ln col 1
lexOne (LState (';' : cs) ln col) = emit KSemicolon cs ln col 1
-- Three dots
lexOne (LState ('.' : '.' : '.' : cs) ln col) = emit KDots cs ln col 3
-- Single dot (for qualified names)
lexOne (LState ('.' : cs) ln col) = emit KDot cs ln col 1
-- Underscore (only if not followed by identifier chars)
lexOne (LState ('_' : cs) ln col)
    | not (any isIdTail (take 1 cs)) = emit KUnderscore cs ln col 1
-- Char literal: #"c"
lexOne (LState ('#' : '"' : c2 : '"' : cs) ln col)
    | c2 /= '\\' = emit (KChar c2) cs ln col 4
lexOne (LState ('#' : '"' : '\\' : e : '"' : cs) ln col) =
    case unesc e of
        Just c2 -> emit (KChar c2) cs ln col 5
        Nothing -> Left $ errAt ln col "Bad character escape"
-- Hash
lexOne (LState ('#' : cs) ln col)
    | not (any isSymChar (take 1 cs)) = emit KHash cs ln col 1
-- Type variables
lexOne (LState ('\'' : '\'' : cs) ln col) =
    let (name, rest) = span isIdTail cs
     in emit (KTyVar (T.pack ("''" ++ name)) True) rest ln col (2 + length name)
lexOne (LState ('\'' : c2 : cs) ln col)
    | isAlpha c2 =
        let (name, rest) = span isIdTail cs
         in emit (KTyVar (T.pack ('\'' : c2 : name)) False) rest ln col (2 + length name)
-- String literals
lexOne (LState ('"' : cs) ln col) = lexString cs ln (col + 1) []
-- Negative numeric literals (SML uses ~ for negation in literals)
lexOne (LState ('~' : c2 : cs) ln col)
    | isDigit c2 =
        let (tok, rest, len) = lexNumber True (c2 : cs)
         in emit tok rest ln col (1 + len)
-- Numeric literals
lexOne (LState (c : cs) ln col)
    | isDigit c =
        let (tok, rest, len) = lexNumber False (c : cs)
         in emit tok rest ln col len
-- Alphanumeric identifiers / keywords
lexOne (LState (c : cs) ln col)
    | isAlpha c || c == '_' =
        let (name, rest) = span isIdTail cs
            full = c : name
            txt = T.pack full
            kind = fromMaybe (KId txt) (lookup txt reserved)
         in emit kind rest ln col (length full)
-- Symbolic identifiers / reserved symbols
lexOne (LState (c : cs) ln col)
    | isSymChar c =
        let (sym, rest) = span isSymChar cs
            full = c : sym
            txt = T.pack full
            kind = case txt of
                "->" -> KArrow
                "=>" -> KDArrow
                "|" -> KBar
                "=" -> KEquals
                ":" -> KColon
                "*" -> KStar
                _ -> KSymId txt
         in emit kind rest ln col (length full)
lexOne (LState (c : _) ln col) =
    Left $ errAt ln col $ "Unexpected character: " ++ show c

emit :: TokenKind -> String -> Int -> Int -> Int -> LResult (Token, LState)
emit k rest ln col len = Right (Token k (Pos ln col), LState rest ln (col + len))

errAt :: Int -> Int -> String -> String
errAt ln col msg = msg ++ " at line " ++ show ln ++ ", column " ++ show col

skipComment :: LState -> Int -> LResult (Token, LState)
skipComment (LState [] ln col) _ = Left $ errAt ln col "Unterminated comment"
skipComment (LState ('*' : ')' : cs) ln col) 1 = lexOne (LState cs ln (col + 2))
skipComment (LState ('*' : ')' : cs) ln col) d = skipComment (LState cs ln (col + 2)) (d - 1)
skipComment (LState ('(' : '*' : cs) ln col) d = skipComment (LState cs ln (col + 2)) (d + 1)
skipComment (LState ('\n' : cs) ln _col) d = skipComment (LState cs (ln + 1) 1) d
skipComment (LState (_ : cs) ln col) d = skipComment (LState cs ln (col + 1)) d

lexString :: String -> Int -> Int -> String -> LResult (Token, LState)
lexString ('"' : cs) ln col acc =
    Right (Token (KString (T.pack (reverse acc))) (Pos ln col), LState cs ln (col + 1))
lexString ('\\' : 'n' : cs) ln col acc = lexString cs ln (col + 2) ('\n' : acc)
lexString ('\\' : 't' : cs) ln col acc = lexString cs ln (col + 2) ('\t' : acc)
lexString ('\\' : '\\' : cs) ln col acc = lexString cs ln (col + 2) ('\\' : acc)
lexString ('\\' : '"' : cs) ln col acc = lexString cs ln (col + 2) ('"' : acc)
lexString ('\\' : _ : _) ln col _ = Left $ errAt ln col "Bad string escape"
lexString [] ln col _ = Left $ errAt ln col "Unterminated string"
lexString (c : cs) ln col acc = lexString cs ln (col + 1) (c : acc)

lexNumber :: Bool -> String -> (TokenKind, String, Int)
lexNumber neg cs =
    let (digits, rest) = span isDigit cs
        n = read digits :: Integer
        val = if neg then negate n else n
        len = length digits
     in case rest of
            ('.' : rest2) ->
                let (frac, rest3) = span isDigit rest2
                    d = read (digits ++ "." ++ (if null frac then "0" else frac)) :: Double
                    dval = if neg then negate d else d
                 in (KReal dval, rest3, len + 1 + length frac)
            _ -> (KInt val, rest, len)

unesc :: Char -> Maybe Char
unesc 'n' = Just '\n'
unesc 't' = Just '\t'
unesc '\\' = Just '\\'
unesc '"' = Just '"'
unesc _ = Nothing
