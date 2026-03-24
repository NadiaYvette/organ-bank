{- | Parser for Erlang source files.
Parses top-level forms (module attributes, function definitions).
-}
module ErlangFrontend.Parser (parseForms) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import ErlangFrontend.AST
import ErlangFrontend.Lexer (Token (..))

-- | Parse a token stream into top-level forms.
parseForms :: [Token] -> Either String [Form]
parseForms = go []
  where
    go acc [] = Right (reverse acc)
    go acc toks = do
        (f, rest) <- parseForm toks
        go (f : acc) rest

parseForm :: [Token] -> Either String (Form, [Token])
parseForm = \case
    -- -module(Name).
    TOp "-" : TAtom "module" : TLParen : TAtom name : TRParen : TDot : rest ->
        Right (FModule name, rest)
    -- -export([...]).
    TOp "-" : TAtom "export" : TLParen : TLBrack : rest -> do
        (exports, rest') <- parseExportList rest
        rest'' <- expectSeq [TRParen, TDot] rest'
        Right (FExport exports, rest'')
    -- -Attr(...).
    TOp "-" : TAtom attr : TLParen : rest -> do
        (args, rest') <- parseArgList rest
        rest'' <- expectSeq [TDot] rest'
        Right (FAttribute attr args, rest'')
    -- Function definition: name(Pat, ...) when Guard -> Body; ... .
    TAtom name : TLParen : rest -> do
        (clauses, rest') <- parseFunClauses name rest
        Right (FFun name clauses, rest')
    t : _ -> Left $ "Unexpected form starting with: " ++ show t
    [] -> Left "Unexpected end of input"

parseExportList :: [Token] -> Either String ([(Text, Int)], [Token])
parseExportList = \case
    TRBrack : rest -> Right ([], rest)
    toks -> go [] toks
  where
    go acc (TAtom name : TOp "/" : TInt arity : rest) =
        case rest of
            TComma : rest' -> go ((name, fromIntegral arity) : acc) rest'
            TRBrack : rest' -> Right (reverse ((name, fromIntegral arity) : acc), rest')
            _ -> Left "Expected ',' or ']' in export list"
    go _ _ = Left "Bad export list entry"

parseFunClauses :: Text -> [Token] -> Either String ([FunClause], [Token])
parseFunClauses name toks = do
    (clause, rest) <- parseFunClauseBody toks
    case rest of
        TSemicolon : TAtom n : TLParen : rest'
            | n == name -> do
                (more, rest'') <- parseFunClauses name rest'
                Right (clause : more, rest'')
        TDot : rest' -> Right ([clause], rest')
        _ -> Left $ "Expected ';' or '.' after function clause for " ++ T.unpack name

parseFunClauseBody :: [Token] -> Either String (FunClause, [Token])
parseFunClauseBody toks = do
    (pats, rest) <- parsePatList toks
    (guard_, rest') <- parseOptGuard rest
    rest'' <- expect TArrow rest'
    (body, rest''') <- parseBody rest''
    Right (FunClause pats guard_ body, rest''')

-- | Parse comma-separated patterns until ')'.
parsePatList :: [Token] -> Either String ([Pat], [Token])
parsePatList = \case
    TRParen : rest -> Right ([], rest)
    toks -> go [] toks
  where
    go acc toks_ = do
        (p, rest) <- parsePat toks_
        case rest of
            TComma : rest' -> go (p : acc) rest'
            TRParen : rest' -> Right (reverse (p : acc), rest')
            _ -> Left "Expected ',' or ')' in pattern list"

parseOptGuard :: [Token] -> Either String (Maybe GuardSeq, [Token])
parseOptGuard = \case
    TKW "when" : rest -> do
        (gs, rest') <- parseGuardSeq rest
        Right (Just gs, rest')
    toks -> Right (Nothing, toks)

-- | Guard sequence: g1, g2 ; g3, g4 → [[g1,g2],[g3,g4]]
parseGuardSeq :: [Token] -> Either String (GuardSeq, [Token])
parseGuardSeq toks = do
    (conj, rest) <- parseGuardConj toks
    case rest of
        TSemicolon : rest' -> do
            (more, rest'') <- parseGuardSeq rest'
            Right (conj : more, rest'')
        _ -> Right ([conj], rest)

parseGuardConj :: [Token] -> Either String ([Expr], [Token])
parseGuardConj toks = do
    (e, rest) <- parseExpr toks
    case rest of
        TComma : rest' -> do
            (more, rest'') <- parseGuardConj rest'
            Right (e : more, rest'')
        _ -> Right ([e], rest)

-- | Parse a comma-separated body (sequence of expressions).
parseBody :: [Token] -> Either String ([Expr], [Token])
parseBody toks = do
    (e, rest) <- parseExpr toks
    case rest of
        TComma : rest' -> do
            (more, rest'') <- parseBody rest'
            Right (e : more, rest'')
        _ -> Right ([e], rest)

-- * Expression parsing

parseExpr :: [Token] -> Either String (Expr, [Token])
parseExpr toks = do
    (lhs, rest) <- parseUnary toks
    parseExprInfix 0 lhs rest

parseUnary :: [Token] -> Either String (Expr, [Token])
parseUnary = \case
    TKW "not" : rest -> do
        (e, rest') <- parsePrimary rest
        Right (EUnOp "not" e, rest')
    TKW "bnot" : rest -> do
        (e, rest') <- parsePrimary rest
        Right (EUnOp "bnot" e, rest')
    TOp "-" : rest -> do
        (e, rest') <- parsePrimary rest
        case e of
            EInt n -> Right (EInt (negate n), rest')
            EFloat d -> Right (EFloat (negate d), rest')
            _ -> Right (EUnOp "-" e, rest')
    toks -> parsePrimary toks

parsePrimary :: [Token] -> Either String (Expr, [Token])
parsePrimary = \case
    [] -> Left "Unexpected end of input"
    TInt n : rest -> Right (EInt n, rest)
    TFloat d : rest -> Right (EFloat d, rest)
    TString s : rest -> Right (EString s, rest)
    TChar c : rest -> Right (EChar c, rest)
    TVar v : TLParen : rest -> do
        (args, rest') <- parseArgList rest
        Right (ECall (EVar v) args, rest')
    TVar v : rest -> Right (EVar v, rest)
    -- Atom possibly followed by ( args ) → function call, or : for remote
    TAtom a : TLParen : rest -> do
        (args, rest') <- parseArgList rest
        Right (ECall (EAtom a) args, rest')
    TAtom a : TOp ":" : TAtom f : TLParen : rest -> do
        (args, rest') <- parseArgList rest
        Right (ECall (ERemote (EAtom a) (EAtom f)) args, rest')
    TAtom a : rest -> Right (EAtom a, rest)
    -- Tuple
    TLBrace : TRBrace : rest -> Right (ETuple [], rest)
    TLBrace : rest -> do
        (es, rest') <- parseCommaSep TRBrace rest
        Right (ETuple es, rest')
    -- List
    TLBrack : TRBrack : rest -> Right (EList [] Nothing, rest)
    TLBrack : rest -> parseListExpr rest
    -- Parenthesised
    TLParen : rest -> do
        (e, rest') <- parseExpr rest
        rest'' <- expect TRParen rest'
        Right (e, rest'')
    -- Keywords
    TKW "case" : rest -> parseCaseExpr rest
    TKW "if" : rest -> parseIfExpr rest
    TKW "receive" : rest -> parseReceiveExpr rest
    TKW "begin" : rest -> parseBeginExpr rest
    TKW "fun" : TAtom name : TOp "/" : TInt arity : rest ->
        Right (EFun name (fromIntegral arity), rest)
    TKW "fun" : TLParen : rest -> parseLambdaExpr rest
    TKW "catch" : rest -> do
        (e, rest') <- parseExpr rest
        Right (ECatch e, rest')
    t : _ -> Left $ "Unexpected token in expression: " ++ show t

parseArgList :: [Token] -> Either String ([Expr], [Token])
parseArgList = \case
    TRParen : rest -> Right ([], rest)
    toks -> parseCommaSepThen TRParen toks

parseCommaSep :: Token -> [Token] -> Either String ([Expr], [Token])
parseCommaSep = parseCommaSepThen

parseCommaSepThen :: Token -> [Token] -> Either String ([Expr], [Token])
parseCommaSepThen close = go []
  where
    go acc toks = do
        (e, rest) <- parseExpr toks
        case rest of
            TComma : rest' -> go (e : acc) rest'
            t : rest'
                | t == close -> Right (reverse (e : acc), rest')
            _ -> Left $ "Expected ',' or " ++ show close

parseListExpr :: [Token] -> Either String (Expr, [Token])
parseListExpr = go []
  where
    go acc toks_ = do
        (e, rest) <- parseExpr toks_
        case rest of
            TComma : rest' -> go (e : acc) rest'
            TBar : rest' -> do
                (tail_, rest'') <- parseExpr rest'
                rest''' <- expect TRBrack rest''
                Right (EList (reverse (e : acc)) (Just tail_), rest''')
            TBarBar : rest' -> do
                -- \|| is list comprehension
                (quals, rest'') <- parseQualifiers rest'
                rest''' <- expect TRBrack rest''
                Right (EListComp e quals, rest''')
            TRBrack : rest' -> Right (EList (reverse (e : acc)) Nothing, rest')
            _ -> Left "Expected ',', '|', '||', or ']' in list"

parseQualifiers :: [Token] -> Either String ([Qualifier], [Token])
parseQualifiers toks = do
    (q, rest) <- parseQualifier toks
    case rest of
        TComma : rest' -> do
            (more, rest'') <- parseQualifiers rest'
            Right (q : more, rest'')
        _ -> Right ([q], rest)

parseQualifier :: [Token] -> Either String (Qualifier, [Token])
parseQualifier toks = do
    -- Try to parse as Pat <- Expr (generator)
    (e, rest) <- parseExpr toks
    case rest of
        TOp "<" : TOp "-" : rest' -> do
            (gen, rest'') <- parseExpr rest'
            Right (QGenerator (exprToPat e) gen, rest'')
        _ -> Right (QFilter e, rest)

parseCaseExpr :: [Token] -> Either String (Expr, [Token])
parseCaseExpr toks = do
    (scrut, rest) <- parseExpr toks
    rest' <- expect (TKW "of") rest
    (clauses, rest'') <- parseCaseClauses rest'
    rest''' <- expect (TKW "end") rest''
    Right (ECase scrut clauses, rest''')

parseIfExpr :: [Token] -> Either String (Expr, [Token])
parseIfExpr toks = do
    (clauses, rest) <- parseIfClauses toks
    rest' <- expect (TKW "end") rest
    Right (EIf clauses, rest')

parseIfClauses :: [Token] -> Either String ([CaseClause], [Token])
parseIfClauses toks = do
    (guard_, rest) <- parseGuardSeq toks
    rest' <- expect TArrow rest
    (body, rest'') <- parseBody rest'
    let clause = CaseClause PWild (Just guard_) body
    case rest'' of
        TSemicolon : rest''' -> do
            (more, rest'''') <- parseIfClauses rest'''
            Right (clause : more, rest'''')
        _ -> Right ([clause], rest'')

parseReceiveExpr :: [Token] -> Either String (Expr, [Token])
parseReceiveExpr toks = do
    (clauses, rest) <- parseCaseClauses toks
    case rest of
        TKW "after" : rest' -> do
            (timeout, rest'') <- parseExpr rest'
            rest''' <- expect TArrow rest''
            (afterBody, rest'''') <- parseBody rest'''
            rest''''' <- expect (TKW "end") rest''''
            Right (EReceive clauses (Just (timeout, afterBody)), rest''''')
        TKW "end" : rest' -> Right (EReceive clauses Nothing, rest')
        _ -> Left "Expected 'after' or 'end' in receive"

parseBeginExpr :: [Token] -> Either String (Expr, [Token])
parseBeginExpr toks = do
    (body, rest) <- parseBody toks
    rest' <- expect (TKW "end") rest
    Right (EBegin body, rest')

parseLambdaExpr :: [Token] -> Either String (Expr, [Token])
parseLambdaExpr toks = do
    (clause, rest) <- parseLambdaClause toks
    case rest of
        TSemicolon : TLParen : rest' ->
            case parseLambdaExpr rest' of
                Right (ELambda more, rest'') -> Right (ELambda (clause : more), rest'')
                Right _ -> Left "Expected lambda expression"
                Left err -> Left err
        TKW "end" : rest' -> Right (ELambda [clause], rest')
        _ -> Left "Expected ';' or 'end' after fun clause"

parseLambdaClause :: [Token] -> Either String (FunClause, [Token])
parseLambdaClause toks = do
    (pats, rest) <- parsePatList toks
    (guard_, rest') <- parseOptGuard rest
    rest'' <- expect TArrow rest'
    (body, rest''') <- parseBody rest''
    Right (FunClause pats guard_ body, rest''')

parseCaseClauses :: [Token] -> Either String ([CaseClause], [Token])
parseCaseClauses toks = do
    (clause, rest) <- parseCaseClause toks
    case rest of
        TSemicolon : rest' -> do
            (more, rest'') <- parseCaseClauses rest'
            Right (clause : more, rest'')
        _ -> Right ([clause], rest)

parseCaseClause :: [Token] -> Either String (CaseClause, [Token])
parseCaseClause toks = do
    (pat, rest) <- parsePat toks
    (guard_, rest') <- parseOptGuard rest
    rest'' <- expect TArrow rest'
    (body, rest''') <- parseBody rest''
    Right (CaseClause pat guard_ body, rest''')

-- * Pattern parsing

parsePat :: [Token] -> Either String (Pat, [Token])
parsePat toks = do
    (lhs, rest) <- parsePatPrimary toks
    parsePatInfix lhs rest

parsePatPrimary :: [Token] -> Either String (Pat, [Token])
parsePatPrimary = \case
    TAtom a : rest -> Right (PAtom a, rest)
    TVar "_" : rest -> Right (PWild, rest)
    TVar v : rest -> Right (PVar v, rest)
    TInt n : rest -> Right (PInt n, rest)
    TFloat d : rest -> Right (PFloat d, rest)
    TString s : rest -> Right (PString s, rest)
    TChar c : rest -> Right (PChar c, rest)
    TOp "-" : TInt n : rest -> Right (PInt (negate n), rest)
    TLBrace : TRBrace : rest -> Right (PTuple [], rest)
    TLBrace : rest -> do
        (ps, rest') <- parsePatCommaSep TRBrace rest
        Right (PTuple ps, rest')
    TLBrack : TRBrack : rest -> Right (PList [] Nothing, rest)
    TLBrack : rest -> parsePatList_ rest
    TLParen : rest -> do
        (p, rest') <- parsePat rest
        rest'' <- expect TRParen rest'
        Right (p, rest'')
    t : _ -> Left $ "Unexpected token in pattern: " ++ show t
    [] -> Left "Unexpected end of input in pattern"

parsePatInfix :: Pat -> [Token] -> Either String (Pat, [Token])
parsePatInfix lhs = \case
    TOp "=" : rest -> do
        (rhs, rest') <- parsePat rest
        Right (PMatch lhs rhs, rest')
    TOp "++" : rest -> do
        (rhs, rest') <- parsePat rest
        Right (PBinOp "++" lhs rhs, rest')
    toks -> Right (lhs, toks)

parsePatList_ :: [Token] -> Either String (Pat, [Token])
parsePatList_ = go []
  where
    go acc toks = do
        (p, rest) <- parsePat toks
        case rest of
            TComma : rest' -> go (p : acc) rest'
            TBar : rest' -> do
                (tail_, rest'') <- parsePat rest'
                rest''' <- expect TRBrack rest''
                Right (PList (reverse (p : acc)) (Just tail_), rest''')
            TRBrack : rest' -> Right (PList (reverse (p : acc)) Nothing, rest')
            _ -> Left "Expected ',', '|', or ']' in pattern list"

parsePatCommaSep :: Token -> [Token] -> Either String ([Pat], [Token])
parsePatCommaSep close = go []
  where
    go acc toks = do
        (p, rest) <- parsePat toks
        case rest of
            TComma : rest' -> go (p : acc) rest'
            t : rest'
                | t == close -> Right (reverse (p : acc), rest')
            _ -> Left $ "Expected ',' or " ++ show close ++ " in pattern"

-- * Expression infix

parseExprInfix :: Int -> Expr -> [Token] -> Either String (Expr, [Token])
parseExprInfix minPrec lhs toks = case toks of
    TOp op : rest
        | Just (prec, assoc) <- Map.lookup op opTable
        , prec >= minPrec -> do
            let rPrec = case assoc of
                    LAssoc -> prec + 1
                    RAssoc -> prec
                    NAssoc -> prec + 1
            (rhs, rest') <- parseUnary rest >>= uncurry (parseExprInfix rPrec)
            parseExprInfix minPrec (mkBinOp op lhs rhs) rest'
    TKW kw : rest
        | Just (prec, assoc) <- Map.lookup kw opTable
        , prec >= minPrec -> do
            let rPrec = case assoc of
                    LAssoc -> prec + 1
                    RAssoc -> prec
                    NAssoc -> prec + 1
            (rhs, rest') <- parseUnary rest >>= uncurry (parseExprInfix rPrec)
            parseExprInfix minPrec (mkBinOp kw lhs rhs) rest'
    TOp "!" : rest
        | 0 >= minPrec -> do
            (rhs, rest') <- parseExpr rest
            parseExprInfix minPrec (ESend lhs rhs) rest'
    TOp "=" : rest
        | 0 >= minPrec -> do
            (rhs, rest') <- parseExpr rest
            Right (EMatch lhs rhs, rest')
    _ -> Right (lhs, toks)

mkBinOp :: Text -> Expr -> Expr -> Expr
mkBinOp = EBinOp

data Assoc = LAssoc | RAssoc | NAssoc

opTable :: Map Text (Int, Assoc)
opTable =
    Map.fromList
        [ ("orelse", (1, RAssoc))
        , ("andalso", (2, RAssoc))
        , ("==", (3, NAssoc))
        , ("/=", (3, NAssoc))
        , ("=:=", (3, NAssoc))
        , ("=/=", (3, NAssoc))
        , ("<", (3, NAssoc))
        , (">", (3, NAssoc))
        , (">=", (3, NAssoc))
        , ("=<", (3, NAssoc))
        , ("++", (4, RAssoc))
        , ("--", (4, RAssoc))
        , ("+", (5, LAssoc))
        , ("-", (5, LAssoc))
        , ("bor", (5, LAssoc))
        , ("bxor", (5, LAssoc))
        , ("bsl", (5, LAssoc))
        , ("bsr", (5, LAssoc))
        , ("or", (5, LAssoc))
        , ("xor", (5, LAssoc))
        , ("*", (6, LAssoc))
        , ("/", (6, LAssoc))
        , ("div", (6, LAssoc))
        , ("rem", (6, LAssoc))
        , ("band", (6, LAssoc))
        , ("and", (6, LAssoc))
        ]

-- * Helpers

expect :: Token -> [Token] -> Either String [Token]
expect expected = \case
    t : rest | t == expected -> Right rest
    t : _ -> Left $ "Expected " ++ show expected ++ ", got: " ++ show t
    [] -> Left $ "Expected " ++ show expected ++ ", got end of input"

expectSeq :: [Token] -> [Token] -> Either String [Token]
expectSeq [] toks = Right toks
expectSeq (e : es) toks = expect e toks >>= expectSeq es

-- | Convert an expression used as a pattern (for generators).
exprToPat :: Expr -> Pat
exprToPat = \case
    EAtom a -> PAtom a
    EVar "_" -> PWild
    EVar v -> PVar v
    EInt n -> PInt n
    EFloat d -> PFloat d
    EString s -> PString s
    EChar c -> PChar c
    ETuple es -> PTuple (map exprToPat es)
    EList es t -> PList (map exprToPat es) (fmap exprToPat t)
    EMatch l r -> PMatch (exprToPat l) (exprToPat r)
    _ -> PWild
