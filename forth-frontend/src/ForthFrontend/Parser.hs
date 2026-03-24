-- | Parser for Forth source.
module ForthFrontend.Parser (parseForth) where

import Data.Text (Text)
import Data.Text qualified as T
import ForthFrontend.AST
import ForthFrontend.Lexer

-- | Parse a sequence of Forth tokens into structured items.
parseForth :: [Token] -> Either String [ForthItem]
parseForth toks = do
    (items, rest) <- parseItems toks
    case rest of
        [] -> Right items
        (t : _) -> Left $ "Unexpected token at top level: " ++ show t

-- | Parse items until end-of-input or a terminator word.
parseItems :: [Token] -> Either String ([ForthItem], [Token])
parseItems = go []
  where
    go acc [] = Right (reverse acc, [])
    go acc toks@(tok : rest) = case tok of
        TColon -> do
            (item, rest') <- parseDef rest
            go (item : acc) rest'
        TSemicolon -> Right (reverse acc, toks) -- terminator for definitions
        TInt n -> go (FLitInt n : acc) rest
        TFloat d -> go (FLitFloat d : acc) rest
        TString s -> go (FLitString s : acc) rest
        TWord w -> parseWordItem acc w rest

    parseWordItem acc w rest = case T.toUpper w of
        "VARIABLE" -> parseDecl FVariable acc rest
        "CONSTANT" -> parseDecl FConstant acc rest
        "IF" -> do
            (item, rest') <- parseIfItem rest
            go (item : acc) rest'
        "BEGIN" -> do
            (item, rest') <- parseBeginItem rest
            go (item : acc) rest'
        "DO" -> do
            (item, rest') <- parseDoItem rest
            go (item : acc) rest'
        -- Control-flow terminators: stop parsing this block
        t | isTerminator t -> Right (reverse acc, TWord w : rest)
        _ -> go (FWord w : acc) rest

    parseDecl con acc (TWord nm : rest) = go (con nm : acc) rest
    parseDecl _ _ _ = Left "Expected name after VARIABLE/CONSTANT"

isTerminator :: Text -> Bool
isTerminator w = w `elem` ["THEN", "ELSE", "UNTIL", "WHILE", "REPEAT", "LOOP", "+LOOP"]

-- | Parse @: name body ;@
parseDef :: [Token] -> Either String (ForthItem, [Token])
parseDef (TWord nm : rest) = do
    (body, rest') <- parseItems rest
    case rest' of
        TSemicolon : rest'' -> Right (FDef nm body, rest'')
        _ -> Left $ "Expected ';' after definition of " ++ T.unpack nm
parseDef _ = Left "Expected name after ':'"

-- | Parse @IF body [ELSE body] THEN@
parseIfItem :: [Token] -> Either String (ForthItem, [Token])
parseIfItem toks = do
    (thenBranch, rest) <- parseItems toks
    case rest of
        TWord w : rest'
            | T.toUpper w == "ELSE" -> do
                (elseBranch, rest'') <- parseItems rest'
                expectWord "THEN" rest'' >>= \rest''' ->
                    Right (FIf thenBranch (Just elseBranch), rest''')
            | T.toUpper w == "THEN" -> Right (FIf thenBranch Nothing, rest')
        _ -> Left "Expected THEN or ELSE after IF"

-- | Parse @BEGIN body UNTIL@ or @BEGIN body WHILE body REPEAT@
parseBeginItem :: [Token] -> Either String (ForthItem, [Token])
parseBeginItem toks = do
    (body1, rest) <- parseItems toks
    case rest of
        TWord w : rest'
            | T.toUpper w == "UNTIL" -> Right (FBeginUntil body1, rest')
            | T.toUpper w == "WHILE" -> do
                (body2, rest'') <- parseItems rest'
                expectWord "REPEAT" rest'' >>= \rest''' ->
                    Right (FBeginWhile body1 body2, rest''')
        _ -> Left "Expected UNTIL or WHILE after BEGIN"

-- | Parse @DO body LOOP@ or @DO body +LOOP@
parseDoItem :: [Token] -> Either String (ForthItem, [Token])
parseDoItem toks = do
    (body, rest) <- parseItems toks
    case rest of
        TWord w : rest'
            | T.toUpper w == "LOOP" || T.toUpper w == "+LOOP" ->
                Right (FDoLoop body, rest')
        _ -> Left "Expected LOOP or +LOOP after DO"

expectWord :: Text -> [Token] -> Either String [Token]
expectWord expected (TWord w : rest)
    | T.toUpper w == expected = Right rest
expectWord expected _ = Left $ "Expected " ++ T.unpack expected
