{- | Parser for Lua 5.4.
Recursive-descent with operator precedence.
-}
module LuaFrontend.Parser (parseLua) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import LuaFrontend.AST
import LuaFrontend.Lexer

-- | Parse a token stream into a block (list of statements).
parseLua :: [Token] -> Either String Block
parseLua toks = do
    (block, rest) <- parseBlock toks
    case rest of
        [] -> Right block
        (t : _) -> Left $ "Unexpected token: " ++ show t

-- | Parse a block of statements.
parseBlock :: [Token] -> Either String (Block, [Token])
parseBlock = go []
  where
    go acc toks = case toks of
        [] -> Right (reverse acc, [])
        (TKW "end" : _) -> Right (reverse acc, toks)
        (TKW "else" : _) -> Right (reverse acc, toks)
        (TKW "elseif" : _) -> Right (reverse acc, toks)
        (TKW "until" : _) -> Right (reverse acc, toks)
        _ -> do
            (s, rest) <- parseStat toks
            go (s : acc) rest

-- | Parse a single statement.
parseStat :: [Token] -> Either String (Stat, [Token])
parseStat = \case
    TSemicolon : rest -> parseStat rest -- skip empty statements
    TKW "local" : TKW "function" : TName nm : TLParen : rest -> do
        (params, vararg, rest') <- parseParamList rest
        (body, rest'') <- parseBlock rest'
        rest''' <- expectKW "end" rest''
        Right (SLocalFunc nm params vararg body, rest''')
    TKW "local" : rest -> parseLocal rest
    TKW "function" : rest -> parseFuncStat rest
    TKW "do" : rest -> do
        (body, rest') <- parseBlock rest
        rest'' <- expectKW "end" rest'
        Right (SDo body, rest'')
    TKW "while" : rest -> do
        (cond, rest') <- parseExpr rest
        rest'' <- expectKW "do" rest'
        (body, rest''') <- parseBlock rest''
        rest'''' <- expectKW "end" rest'''
        Right (SWhile cond body, rest'''')
    TKW "repeat" : rest -> do
        (body, rest') <- parseBlock rest
        rest'' <- expectKW "until" rest'
        (cond, rest''') <- parseExpr rest''
        Right (SRepeat body cond, rest''')
    TKW "if" : rest -> parseIfStat rest
    TKW "for" : rest -> parseForStat rest
    TKW "return" : rest -> parseReturn rest
    TKW "break" : rest -> Right (SBreak, rest)
    toks -> parseExprStat toks

-- | Parse local name [, name ...] [= expr, expr ...]
parseLocal :: [Token] -> Either String (Stat, [Token])
parseLocal toks = do
    (names, rest) <- parseNameList toks
    case rest of
        TEq : rest' -> do
            (exprs, rest'') <- parseExprList rest'
            Right (SLocal names exprs, rest'')
        _ -> Right (SLocal names [], rest)

-- | Parse function name(params) body end
parseFuncStat :: [Token] -> Either String (Stat, [Token])
parseFuncStat toks = do
    (fname, rest) <- parseFuncName toks
    rest' <- expect TLParen rest
    (params, vararg, rest'') <- parseParamList rest'
    (body, rest''') <- parseBlock rest''
    rest'''' <- expectKW "end" rest'''
    Right (SFunc fname params vararg body, rest'''')

parseFuncName :: [Token] -> Either String (FuncName, [Token])
parseFuncName (TName n : rest) = go [n] rest
  where
    go acc (TDot : TName n' : rest') = go (n' : acc) rest'
    go acc (TColon : TName n' : rest') = Right (FuncName (reverse acc) (Just n'), rest')
    go acc rest' = Right (FuncName (reverse acc) Nothing, rest')
parseFuncName _ = Left "Expected function name"

parseNameList :: [Token] -> Either String ([Text], [Token])
parseNameList (TName n : rest) = go [n] rest
  where
    go acc (TComma : TName n' : rest') = go (n' : acc) rest'
    go acc rest' = Right (reverse acc, rest')
parseNameList _ = Left "Expected name"

-- | Parse if cond then block {elseif cond then block} [else block] end
parseIfStat :: [Token] -> Either String (Stat, [Token])
parseIfStat toks = do
    (cond, rest) <- parseExpr toks
    rest' <- expectKW "then" rest
    (body, rest'') <- parseBlock rest'
    (elseifs, elsePart, rest''') <- parseElseIfs rest''
    rest'''' <- expectKW "end" rest'''
    Right (SIf cond body elseifs elsePart, rest'''')

parseElseIfs :: [Token] -> Either String ([(Expr, Block)], Maybe Block, [Token])
parseElseIfs (TKW "elseif" : rest) = do
    (cond, rest') <- parseExpr rest
    rest'' <- expectKW "then" rest'
    (body, rest''') <- parseBlock rest''
    (more, elsePart, rest'''') <- parseElseIfs rest'''
    Right ((cond, body) : more, elsePart, rest'''')
parseElseIfs (TKW "else" : rest) = do
    (body, rest') <- parseBlock rest
    Right ([], Just body, rest')
parseElseIfs rest = Right ([], Nothing, rest)

-- | Parse for statement (numeric or generic).
parseForStat :: [Token] -> Either String (Stat, [Token])
parseForStat (TName n : TEq : rest) = do
    -- Numeric for: for i = start, stop [, step] do body end
    (start, rest') <- parseExpr rest
    rest'' <- expect TComma rest'
    (stop, rest''') <- parseExpr rest''
    (step, rest'''') <- case rest''' of
        TComma : rest4 -> do
            (s, rest5) <- parseExpr rest4
            Right (Just s, rest5)
        _ -> Right (Nothing, rest''')
    rest5 <- expectKW "do" rest''''
    (body, rest6) <- parseBlock rest5
    rest7 <- expectKW "end" rest6
    Right (SForNum n start stop step body, rest7)
parseForStat toks = do
    -- Generic for: for names in exprs do body end
    (names, rest) <- parseNameList toks
    rest' <- expectKW "in" rest
    (exprs, rest'') <- parseExprList rest'
    rest''' <- expectKW "do" rest''
    (body, rest'''') <- parseBlock rest'''
    rest5 <- expectKW "end" rest''''
    Right (SForIn names exprs body, rest5)

-- | Parse return [exprlist]
parseReturn :: [Token] -> Either String (Stat, [Token])
parseReturn toks = case toks of
    [] -> Right (SReturn [], [])
    TSemicolon : rest -> Right (SReturn [], rest)
    (TKW "end" : _) -> Right (SReturn [], toks)
    (TKW "else" : _) -> Right (SReturn [], toks)
    (TKW "elseif" : _) -> Right (SReturn [], toks)
    (TKW "until" : _) -> Right (SReturn [], toks)
    _ -> do
        (exprs, rest) <- parseExprList toks
        let rest' = case rest of TSemicolon : r -> r; _ -> rest
        Right (SReturn exprs, rest')

-- | Parse expression-statement (assignment or function call).
parseExprStat :: [Token] -> Either String (Stat, [Token])
parseExprStat toks = do
    (expr, rest) <- parseSuffixExpr toks
    case rest of
        TEq : rest' -> do
            -- Single assignment: lhs = rhs
            (rhs, rest'') <- parseExprList rest'
            Right (SAssign [exprToLVar expr] rhs, rest'')
        TComma : rest' -> do
            -- Multi-assignment: lhs1, lhs2 = rhs1, rhs2
            (moreVars, rest'') <- parseLVarList rest'
            rest''' <- expect TEq rest''
            (rhs, rest'''') <- parseExprList rest'''
            Right (SAssign (exprToLVar expr : moreVars) rhs, rest'''')
        _ -> Right (SExprStat expr, rest)

parseLVarList :: [Token] -> Either String ([LVar], [Token])
parseLVarList toks = do
    (expr, rest) <- parseSuffixExpr toks
    case rest of
        TComma : rest' -> do
            (more, rest'') <- parseLVarList rest'
            Right (exprToLVar expr : more, rest'')
        _ -> Right ([exprToLVar expr], rest)

exprToLVar :: Expr -> LVar
exprToLVar (EName n) = LVName n
exprToLVar (EIndex e k) = LVIndex e k
exprToLVar (EField e f) = LVField e f
exprToLVar _ = LVName "_" -- fallback

-- * Expression parsing

parseExprList :: [Token] -> Either String ([Expr], [Token])
parseExprList toks = do
    (e, rest) <- parseExpr toks
    case rest of
        TComma : rest' -> do
            (more, rest'') <- parseExprList rest'
            Right (e : more, rest'')
        _ -> Right ([e], rest)

-- | Parse an expression with operator precedence.
parseExpr :: [Token] -> Either String (Expr, [Token])
parseExpr toks = do
    (lhs, rest) <- parseUnary toks
    parseInfix 0 lhs rest

parseUnary :: [Token] -> Either String (Expr, [Token])
parseUnary = \case
    TKW "not" : rest -> do
        (e, rest') <- parseUnary rest
        Right (EUnOp "not" e, rest')
    THash : rest -> do
        (e, rest') <- parseUnary rest
        Right (EUnOp "#" e, rest')
    TOp "-" : rest -> do
        (e, rest') <- parseUnary rest
        Right (EUnOp "-" e, rest')
    TOp "~" : rest -> do
        (e, rest') <- parseUnary rest
        Right (EUnOp "~" e, rest')
    toks -> parseSuffixExpr toks

-- | Parse a primary expression with suffix (calls, indexing, fields, methods).
parseSuffixExpr :: [Token] -> Either String (Expr, [Token])
parseSuffixExpr toks = do
    (e, rest) <- parsePrimary toks
    parseSuffix e rest

parseSuffix :: Expr -> [Token] -> Either String (Expr, [Token])
parseSuffix e = \case
    TLParen : rest -> do
        (args, rest') <- parseCallArgs rest
        parseSuffix (ECall e args) rest'
    TLBrace : rest -> do
        (fields, rest') <- parseTableFields rest
        parseSuffix (ECall e [ETable fields]) rest'
    TString s : rest -> parseSuffix (ECall e [EString s]) rest
    TDot : TName n : rest -> parseSuffix (EField e n) rest
    TLBrack : rest -> do
        (idx, rest') <- parseExpr rest
        rest'' <- expect TRBrack rest'
        parseSuffix (EIndex e idx) rest''
    TColon : TName n : TLParen : rest -> do
        (args, rest') <- parseCallArgs rest
        parseSuffix (EMethodCall e n args) rest'
    TColon : TName n : TLBrace : rest -> do
        (fields, rest') <- parseTableFields rest
        parseSuffix (EMethodCall e n [ETable fields]) rest'
    TColon : TName n : TString s : rest ->
        parseSuffix (EMethodCall e n [EString s]) rest
    rest -> Right (e, rest)

parsePrimary :: [Token] -> Either String (Expr, [Token])
parsePrimary = \case
    TKW "nil" : rest -> Right (ENil, rest)
    TKW "true" : rest -> Right (ETrue, rest)
    TKW "false" : rest -> Right (EFalse, rest)
    TInt n : rest -> Right (EInt n, rest)
    TFloat d : rest -> Right (EFloat d, rest)
    TString s : rest -> Right (EString s, rest)
    TDotDotDot : rest -> Right (EVarArg, rest)
    TName n : rest -> Right (EName n, rest)
    TLParen : rest -> do
        (e, rest') <- parseExpr rest
        rest'' <- expect TRParen rest'
        Right (e, rest'')
    TKW "function" : TLParen : rest -> do
        (params, vararg, rest') <- parseParamList rest
        (body, rest'') <- parseBlock rest'
        rest''' <- expectKW "end" rest''
        Right (EFunc params vararg body, rest''')
    TLBrace : rest -> do
        (fields, rest') <- parseTableFields rest
        Right (ETable fields, rest')
    t : _ -> Left $ "Unexpected token in expression: " ++ show t
    [] -> Left "Unexpected end of input in expression"

parseCallArgs :: [Token] -> Either String ([Expr], [Token])
parseCallArgs = \case
    TRParen : rest -> Right ([], rest)
    toks -> do
        (exprs, rest) <- parseExprList toks
        rest' <- expect TRParen rest
        Right (exprs, rest')

-- | Parse parameter list including closing ')'.
parseParamList :: [Token] -> Either String ([Text], Bool, [Token])
parseParamList = \case
    TRParen : rest -> Right ([], False, rest)
    TDotDotDot : TRParen : rest -> Right ([], True, rest)
    toks -> do
        (names, rest) <- parseNameList toks
        case rest of
            TComma : TDotDotDot : TRParen : rest' -> Right (names, True, rest')
            TRParen : rest' -> Right (names, False, rest')
            _ -> Left "Expected ')' or '...' in parameter list"

-- | Parse table constructor fields until '}'.
parseTableFields :: [Token] -> Either String ([Field], [Token])
parseTableFields = go []
  where
    go acc (TRBrace : rest) = Right (reverse acc, rest)
    go acc toks = do
        (f, rest) <- parseField toks
        let rest' = case rest of
                TComma : r -> r
                TSemicolon : r -> r
                _ -> rest
        go (f : acc) rest'

parseField :: [Token] -> Either String (Field, [Token])
parseField = \case
    TLBrack : rest -> do
        (key, rest') <- parseExpr rest
        rest'' <- expect TRBrack rest'
        rest''' <- expect TEq rest''
        (val, rest'''') <- parseExpr rest'''
        Right (FKey key val, rest'''')
    TName n : TEq : rest -> do
        (val, rest') <- parseExpr rest
        Right (FName n val, rest')
    toks -> do
        (e, rest) <- parseExpr toks
        Right (FExpr e, rest)

-- * Operator precedence

data Assoc = LAssoc | RAssoc

opTable :: Map.Map Text (Int, Assoc)
opTable =
    Map.fromList
        [ ("or", (1, LAssoc))
        , ("and", (2, LAssoc))
        , ("<", (3, LAssoc))
        , (">", (3, LAssoc))
        , ("<=", (3, LAssoc))
        , (">=", (3, LAssoc))
        , ("~=", (3, LAssoc))
        , ("==", (3, LAssoc))
        , ("|", (4, LAssoc))
        , ("~", (5, LAssoc))
        , ("&", (6, LAssoc))
        , ("<<", (7, LAssoc))
        , (">>", (7, LAssoc))
        , ("..", (8, RAssoc))
        , ("+", (9, LAssoc))
        , ("-", (9, LAssoc))
        , ("*", (10, LAssoc))
        , ("/", (10, LAssoc))
        , ("//", (10, LAssoc))
        , ("%", (10, LAssoc))
        , ("^", (12, RAssoc))
        ]

parseInfix :: Int -> Expr -> [Token] -> Either String (Expr, [Token])
parseInfix minPrec lhs = \case
    TOp op : rest
        | Just (prec, assoc) <- Map.lookup op opTable
        , prec >= minPrec -> do
            let rPrec = case assoc of LAssoc -> prec + 1; RAssoc -> prec
            (rhs, rest') <- parseUnary rest >>= uncurry (parseInfix rPrec)
            parseInfix minPrec (EBinOp op lhs rhs) rest'
    TDotDot : rest
        | 8 >= minPrec -> do
            (rhs, rest') <- parseUnary rest >>= uncurry (parseInfix 8)
            parseInfix minPrec (EBinOp ".." lhs rhs) rest'
    TKW "and" : rest
        | 2 >= minPrec -> do
            (rhs, rest') <- parseUnary rest >>= uncurry (parseInfix 3)
            parseInfix minPrec (EBinOp "and" lhs rhs) rest'
    TKW "or" : rest
        | 1 >= minPrec -> do
            (rhs, rest') <- parseUnary rest >>= uncurry (parseInfix 2)
            parseInfix minPrec (EBinOp "or" lhs rhs) rest'
    rest -> Right (lhs, rest)

-- * Helpers

expect :: Token -> [Token] -> Either String [Token]
expect t (t' : rest) | t == t' = Right rest
expect t _ = Left $ "Expected " ++ show t

expectKW :: Text -> [Token] -> Either String [Token]
expectKW kw (TKW kw' : rest) | kw == kw' = Right rest
expectKW kw _ = Left $ "Expected '" ++ T.unpack kw ++ "'"
