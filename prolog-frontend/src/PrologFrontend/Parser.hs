{- | Parser: token stream → Prolog terms and sentences.
Handles standard ISO operator precedence.
-}
module PrologFrontend.Parser (parseSentences) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import PrologFrontend.Lexer (Token (..))
import PrologFrontend.Term

-- | Parse a token stream into a list of sentences.
parseSentences :: [Token] -> Either String [Sentence]
parseSentences = go []
  where
    go acc [] = Right (reverse acc)
    go acc toks = do
        (s, rest) <- parseSentence toks
        go (s : acc) rest

parseSentence :: [Token] -> Either String (Sentence, [Token])
parseSentence = \case
    -- Directive: :- Goal.
    TOpAtom ":-" : rest -> do
        (body, rest') <- parseTerm 1200 rest
        expectDot rest' >>= \r -> Right (SDirective body, r)
    -- Query: ?- Goal.
    TOpAtom "?-" : rest -> do
        (body, rest') <- parseTerm 1200 rest
        expectDot rest' >>= \r -> Right (SQuery body, r)
    toks -> do
        (head_, rest) <- parseTerm 999 toks
        case rest of
            -- Clause: Head :- Body.
            TOpAtom ":-" : rest' -> do
                (body, rest'') <- parseTerm 1200 rest'
                expectDot rest'' >>= \r -> Right (SClause head_ (Just body), r)
            -- DCG rule: Head --> Body.  (stored as directive for now)
            TOpAtom "-->" : rest' -> do
                (body, rest'') <- parseTerm 1200 rest'
                expectDot rest'' >>= \r -> Right (SClause (TmCompound "-->" [head_, body]) Nothing, r)
            -- Fact: Head.
            _ -> expectDot rest >>= \r -> Right (SClause head_ Nothing, r)

expectDot :: [Token] -> Either String [Token]
expectDot = \case
    TDot : rest -> Right rest
    [] -> Left "Expected '.' at end of clause"
    t : _ -> Left $ "Expected '.', got: " ++ show t

{- | Operator-precedence term parser.
prec is the maximum precedence allowed at this level.
-}
parseTerm :: Int -> [Token] -> Either String (Term, [Token])
parseTerm maxPrec toks = do
    (lhs, rest) <- parsePrimary toks
    parseInfix maxPrec lhs rest

parsePrimary :: [Token] -> Either String (Term, [Token])
parsePrimary = \case
    [] -> Left "Unexpected end of input"
    TInt n : rest -> Right (TmInt n, rest)
    TFloat d : rest -> Right (TmFloat d, rest)
    TString s : rest -> Right (TmString s, rest)
    TVar v : rest -> Right (TmVar v, rest)
    -- Negative number
    TOpAtom "-" : TInt n : rest -> Right (TmInt (negate n), rest)
    TOpAtom "-" : TFloat d : rest -> Right (TmFloat (negate d), rest)
    -- Prefix operator \+
    TOpAtom op : rest
        | Just (prec, _) <- Map.lookup op prefixOps -> do
            (arg, rest') <- parseTerm (prec - 1) rest
            Right (TmCompound op [arg], rest')
    -- Atom possibly followed by ( args )
    TAtom a : TLParen : rest -> do
        (args, rest') <- parseArgs rest
        Right (TmCompound a args, rest')
    TAtom a : rest -> Right (TmAtom a, rest)
    -- List
    TLBrack : TRBrack : rest -> Right (TmList [] Nothing, rest)
    TLBrack : rest -> do
        (elts, tail_, rest') <- parseListElts rest
        Right (TmList elts tail_, rest')
    -- Parenthesised
    TLParen : rest -> do
        (t, rest') <- parseTerm 1200 rest
        case rest' of
            TRParen : rest'' -> Right (t, rest'')
            _ -> Left "Expected ')'"
    -- Curly braces: {X} → '{}'(X)
    TLBrace : TRBrace : rest -> Right (TmAtom "{}", rest)
    TLBrace : rest -> do
        (t, rest') <- parseTerm 1200 rest
        case rest' of
            TRBrace : rest'' -> Right (TmCompound "{}" [t], rest'')
            _ -> Left "Expected '}'"
    t : _ -> Left $ "Unexpected token: " ++ show t

parseInfix :: Int -> Term -> [Token] -> Either String (Term, [Token])
parseInfix maxPrec lhs toks = case toks of
    TOpAtom op : rest
        | Just (prec, assoc) <- Map.lookup op infixOps
        , prec <= maxPrec ->
            infixWith op prec assoc rest
    -- Alphabetic infix operators: is, mod, rem
    TAtom op : rest
        | Just (prec, assoc) <- Map.lookup op infixOps
        , prec <= maxPrec ->
            infixWith op prec assoc rest
    TComma : rest
        | 1000 <= maxPrec -> do
            (rhs, rest') <- parseTerm 1000 rest
            parseInfix maxPrec (TmCompound "," [lhs, rhs]) rest'
    TAtom ";" : rest
        | 1100 <= maxPrec -> do
            (rhs, rest') <- parseTerm 1100 rest
            parseInfix maxPrec (TmCompound ";" [lhs, rhs]) rest'
    _ -> Right (lhs, toks)
  where
    infixWith op prec assoc rest = do
        let rPrec = case assoc of
                LeftAssoc -> prec - 1
                RightAssoc -> prec
                NonAssoc -> prec - 1
        (rhs, rest') <- parseTerm rPrec rest
        parseInfix maxPrec (TmCompound op [lhs, rhs]) rest'

parseArgs :: [Token] -> Either String ([Term], [Token])
parseArgs = \case
    TRParen : rest -> Right ([], rest)
    toks -> go [] toks
  where
    go acc toks_ = do
        (t, rest) <- parseTerm 999 toks_
        case rest of
            TComma : rest' -> go (t : acc) rest'
            TRParen : rest' -> Right (reverse (t : acc), rest')
            _ -> Left "Expected ',' or ')' in argument list"

parseListElts :: [Token] -> Either String ([Term], Maybe Term, [Token])
parseListElts = go []
  where
    go acc toks_ = do
        (t, rest) <- parseTerm 999 toks_
        case rest of
            TComma : rest' -> go (t : acc) rest'
            TBar : rest' -> do
                (tail_, rest'') <- parseTerm 999 rest'
                case rest'' of
                    TRBrack : rest''' -> Right (reverse (t : acc), Just tail_, rest''')
                    _ -> Left "Expected ']' after list tail"
            TRBrack : rest' -> Right (reverse (t : acc), Nothing, rest')
            _ -> Left "Expected ',', '|', or ']' in list"

-- * Operator tables (ISO Prolog standard)

data Assoc = LeftAssoc | RightAssoc | NonAssoc

-- | Infix operators: name → (precedence, associativity).
infixOps :: Map Text (Int, Assoc)
infixOps =
    Map.fromList
        [ (":-", (1200, NonAssoc))
        , ("-->", (1200, NonAssoc))
        , ("->", (1050, RightAssoc))
        , ("=", (700, NonAssoc))
        , ("\\=", (700, NonAssoc))
        , ("==", (700, NonAssoc))
        , ("\\==", (700, NonAssoc))
        , ("is", (700, NonAssoc))
        , ("=:=", (700, NonAssoc))
        , ("=\\=", (700, NonAssoc))
        , ("<", (700, NonAssoc))
        , (">", (700, NonAssoc))
        , (">=", (700, NonAssoc))
        , ("=<", (700, NonAssoc))
        , ("@<", (700, NonAssoc))
        , ("@>", (700, NonAssoc))
        , ("@>=", (700, NonAssoc))
        , ("@=<", (700, NonAssoc))
        , ("=..", (700, NonAssoc))
        , ("+", (500, LeftAssoc))
        , ("-", (500, LeftAssoc))
        , ("/\\", (500, LeftAssoc))
        , ("\\/", (500, LeftAssoc))
        , ("*", (400, LeftAssoc))
        , ("/", (400, LeftAssoc))
        , ("//", (400, LeftAssoc))
        , ("rem", (400, LeftAssoc))
        , ("mod", (400, LeftAssoc))
        , ("**", (200, NonAssoc))
        , ("^", (200, RightAssoc))
        ]

-- | Prefix operators: name → (precedence, subterm precedence).
prefixOps :: Map Text (Int, Int)
prefixOps =
    Map.fromList
        [ ("\\+", (900, 900))
        , ("not", (900, 900))
        , ("-", (200, 200))
        , ("+", (200, 200))
        , ("\\", (200, 200))
        ]
