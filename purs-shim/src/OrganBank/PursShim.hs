{- | PureScript CoreFn Extraction Shim

Runs @purs compile --dump-corefn@ on a PureScript source file,
reads the resulting corefn.json, and emits OrganIR JSON.

CoreFn JSON is parsed manually (no aeson) for GHC 9.14 compatibility.
-}
module OrganBank.PursShim (
    extractOrganIR,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Directory (doesFileExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName)
import System.Process (readProcessWithExitCode)

-- | Extract PureScript CoreFn from a .purs file and emit OrganIR JSON.
extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
    -- Step 1: Run purs compile --dump-corefn
    (exitCode, _stdout, stderrOut) <-
        readProcessWithExitCode "purs" ["compile", "--dump-corefn", inputPath] ""
    case exitCode of
        ExitFailure code ->
            pure $
                Left $
                    "purs compile failed (exit " <> show code <> "): " <> stderrOut
        ExitSuccess -> do
            -- Step 2: Find the corefn.json in output/<Module>/corefn.json
            -- The module name is typically the PascalCase of the filename
            mCoreFnPath <- findCoreFnJson inputPath
            case mCoreFnPath of
                Nothing -> pure $ Left "Could not find corefn.json in output/"
                Just coreFnPath -> do
                    -- Step 3: Read and parse the CoreFn JSON
                    coreFnText <- TIO.readFile coreFnPath
                    let organIR = coreFnToOrganIR inputPath coreFnText
                    pure $ Right organIR

{- | Find the corefn.json file produced by purs compile.
Searches output/<dir>/corefn.json for any directory.
Prefers a directory matching the source filename.
-}
findCoreFnJson :: FilePath -> IO (Maybe FilePath)
findCoreFnJson inputPath = do
    let expectedModule = takeBaseName inputPath
        expectedPath = "output/" <> expectedModule <> "/corefn.json"
    exists <- doesFileExist expectedPath
    if exists
        then pure (Just expectedPath)
        else do
            -- Fall back: scan output/ for any corefn.json
            dirs <- listDirectory "output"
            findFirst dirs
  where
    findFirst [] = pure Nothing
    findFirst (d : ds) = do
        let p = "output/" <> d <> "/corefn.json"
        exists <- doesFileExist p
        if exists then pure (Just p) else findFirst ds

-- | Transform CoreFn JSON text to OrganIR JSON.
coreFnToOrganIR :: FilePath -> Text -> Text
coreFnToOrganIR sourceFile coreFnText =
    let modName_ = extractJsonString "moduleName" coreFnText
        exports = extractJsonStringArray "exports" coreFnText
        decls = extractDecls coreFnText
        metadata =
            IR.Metadata
                IR.LPurescript
                Nothing
                (Just (T.pack sourceFile))
                "purs-shim-0.1"
                Nothing
        module_ =
            IR.Module
                modName_
                exports
                (zipWith (declToIR modName_) [1 ..] decls)
                []
                []
     in renderOrganIR (IR.OrganIR metadata module_)

-- -----------------------------------------------------------------------
-- Minimal JSON text extraction (no aeson)
-- -----------------------------------------------------------------------

-- | A declaration extracted from CoreFn.
data CoreFnDecl = CoreFnDecl
    { declIdent :: Text
    , declArity :: Int
    -- ^ Number of nested Abs nodes (0 for values)
    , _declIsRec :: Bool
    , declExprText :: Text
    -- ^ Raw JSON text of the "expression" field
    }

{- | Extract a top-level JSON string value by key.
Looks for "key": "value" and returns value.
-}
extractJsonString :: Text -> Text -> Text
extractJsonString key txt =
    let needle = "\"" <> key <> "\""
     in case T.breakOn needle txt of
            (_, rest)
                | T.null rest -> ""
                | otherwise ->
                    let afterKey = T.drop (T.length needle) rest
                        -- skip whitespace and colon
                        afterColon = T.dropWhile (\c -> c == ' ' || c == ':' || c == '\n' || c == '\r' || c == '\t') afterKey
                     in case T.uncons afterColon of
                            Just ('"', valRest) -> extractQuotedString valRest
                            -- Handle array of strings as module name (CoreFn uses array form)
                            Just ('[', arrRest) ->
                                let _trimmed = T.dropWhile (\c -> c == ' ' || c == '"' || c == '\n' || c == '\r' || c == '\t') arrRest
                                 in T.intercalate "." $
                                                T.splitOn "\",\"" $
                                                    T.filter (\c -> c /= ' ' && c /= '\n' && c /= '\r' && c /= '\t') $
                                                        takeUntilChar ']' (T.dropWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t') arrRest)
                            _ -> ""

{- | Extract a JSON string array value by key.
Looks for "key": ["a", "b", ...] and returns the list.
-}
extractJsonStringArray :: Text -> Text -> [Text]
extractJsonStringArray key txt =
    let needle = "\"" <> key <> "\""
     in case T.breakOn needle txt of
            (_, rest)
                | T.null rest -> []
                | otherwise ->
                    let afterKey = T.drop (T.length needle) rest
                        afterColon = T.dropWhile (\c -> c == ' ' || c == ':' || c == '\n' || c == '\r' || c == '\t') afterKey
                     in case T.uncons afterColon of
                            Just ('[', arrRest) -> parseStringArray arrRest
                            _ -> []

-- | Parse a JSON string array body (after the opening '[').
parseStringArray :: Text -> [Text]
parseStringArray = go . T.strip
  where
    go txt
        | T.null txt = []
        | T.head txt == ']' = []
        | T.head txt == '"' =
            let val = extractQuotedString (T.tail txt)
                rest = T.drop (T.length val + 2) txt -- skip "val"
                rest' = T.dropWhile (\c -> c == ' ' || c == ',' || c == '\n' || c == '\r' || c == '\t') rest
             in val : go rest'
        | otherwise =
            let rest = T.dropWhile (\c -> c /= '"' && c /= ']') txt
             in go rest

-- | Extract the string content up to the next unescaped double quote.
extractQuotedString :: Text -> Text
extractQuotedString = go T.empty
  where
    go acc txt = case T.uncons txt of
        Nothing -> acc
        Just ('\\', rest) -> case T.uncons rest of
            Just (c, rest') -> go (acc <> T.singleton c) rest'
            Nothing -> acc
        Just ('"', _) -> acc
        Just (c, rest) -> go (acc <> T.singleton c) rest

-- | Take characters until a specific char is found (not included).
takeUntilChar :: Char -> Text -> Text
takeUntilChar c = T.takeWhile (/= c)

{- | Extract declarations from the CoreFn JSON "decls" array.
CoreFn decls have the form:
  [{"bindType": "NonRec", "identifier": "foo", "expression": {...}}, ...]
For Rec bindings:
  [{"bindType": "Rec", "binds": [{"identifier": "bar", "expression": {...}}, ...]}]
-}
extractDecls :: Text -> [CoreFnDecl]
extractDecls txt =
    let needle = "\"decls\""
     in case T.breakOn needle txt of
            (_, rest)
                | T.null rest -> []
                | otherwise ->
                    let afterKey = T.drop (T.length needle) rest
                        afterColon = T.dropWhile (\c -> c == ' ' || c == ':' || c == '\n' || c == '\r' || c == '\t') afterKey
                     in case T.uncons afterColon of
                            Just ('[', declsBody) -> parseDeclArray declsBody
                            _ -> []

-- | Parse the declarations array body.
parseDeclArray :: Text -> [CoreFnDecl]
parseDeclArray = go
  where
    go txt
        | T.null txt = []
        | otherwise =
            let stripped = T.strip txt
             in case T.uncons stripped of
                    Nothing -> []
                    Just (']', _) -> []
                    Just ('{', _objRest) ->
                        let (obj, afterObj) = extractBracedBlock '{' '}' stripped
                            bindType = extractJsonString "bindType" obj
                            decls
                                | bindType == "NonRec" =
                                    let ident = extractJsonString "identifier" obj
                                        arity = countAbsNodes obj
                                        exprTxt = extractExprField obj
                                     in [CoreFnDecl ident arity False exprTxt]
                                | bindType == "Rec" = extractRecBinds obj
                                | otherwise = []
                            rest = T.dropWhile (\c -> c == ' ' || c == ',' || c == '\n' || c == '\r' || c == '\t') afterObj
                         in decls ++ go rest
                    Just (_, rest) -> go rest -- skip unexpected chars

-- | Extract binds from a Rec declaration.
extractRecBinds :: Text -> [CoreFnDecl]
extractRecBinds obj =
    let needle = "\"binds\""
     in case T.breakOn needle obj of
            (_, rest)
                | T.null rest -> []
                | otherwise ->
                    let afterKey = T.drop (T.length needle) rest
                        afterColon = T.dropWhile (\c -> c == ' ' || c == ':' || c == '\n' || c == '\r' || c == '\t') afterKey
                     in case T.uncons afterColon of
                            Just ('[', bindsBody) -> parseBindArray bindsBody
                            _ -> []

-- | Parse individual binds in a Rec binding.
parseBindArray :: Text -> [CoreFnDecl]
parseBindArray = go
  where
    go txt
        | T.null txt = []
        | otherwise =
            let stripped = T.strip txt
             in case T.uncons stripped of
                    Nothing -> []
                    Just (']', _) -> []
                    Just ('{', _) ->
                        let (obj, afterObj) = extractBracedBlock '{' '}' stripped
                            ident = extractJsonString "identifier" obj
                            arity = countAbsNodes obj
                            exprTxt = extractExprField obj
                            rest = T.dropWhile (\c -> c == ' ' || c == ',' || c == '\n' || c == '\r' || c == '\t') afterObj
                         in CoreFnDecl ident arity True exprTxt : go rest
                    Just (_, rest) -> go rest

{- | Count nested Abs nodes in an expression to determine arity.
Looks for "type": "Abs" patterns in the expression.
-}
countAbsNodes :: Text -> Int
countAbsNodes txt = countNestedAbs txt 0
  where
    -- Find the expression field, then count leading Abs nodes
    countNestedAbs t depth =
        let needle = "\"Abs\""
         in case T.breakOn needle t of
                (before, rest)
                    | T.null rest -> depth
                    | otherwise ->
                        -- Verify this is a "type": "Abs" pattern (not some random "Abs")
                        let contextCheck = T.takeEnd 30 before
                         in if "\"type\"" `T.isInfixOf` contextCheck
                                || "\"type\" :" `T.isInfixOf` contextCheck
                                then countNestedAbs (T.drop (T.length needle) rest) (depth + 1)
                                else countNestedAbs (T.drop (T.length needle) rest) depth

{- | Extract a brace-delimited block, handling nesting.
Returns (the block including braces, the text after the block).
-}
extractBracedBlock :: Char -> Char -> Text -> (Text, Text)
extractBracedBlock open close = go 0 T.empty False
  where
    go :: Int -> Text -> Bool -> Text -> (Text, Text)
    go depth acc inString txt = case T.uncons txt of
        Nothing -> (acc, T.empty)
        Just ('\\', rest) | inString -> case T.uncons rest of
            Just (c, rest') -> go depth (acc <> "\\" <> T.singleton c) True rest'
            Nothing -> (acc <> "\\", T.empty)
        Just ('"', rest) -> go depth (acc <> "\"") (not inString) rest
        Just (c, rest)
            | not inString && c == open ->
                let depth' = depth + 1
                 in go depth' (acc <> T.singleton c) False rest
            | not inString && c == close ->
                if depth <= 1
                    then (acc <> T.singleton c, rest)
                    else go (depth - 1) (acc <> T.singleton c) False rest
            | otherwise -> go depth (acc <> T.singleton c) inString rest

-- -----------------------------------------------------------------------
-- Expression field extraction
-- -----------------------------------------------------------------------

{- | Extract the "expression" field value (a JSON object) from a declaration object.
Finds "expression" key and extracts the braced block.
-}
extractExprField :: Text -> Text
extractExprField obj =
    let needle = "\"expression\""
     in case T.breakOn needle obj of
            (_, rest)
                | T.null rest -> ""
                | otherwise ->
                    let afterKey = T.drop (T.length needle) rest
                        afterColon = T.dropWhile (\c -> c == ' ' || c == ':' || c == '\n' || c == '\r' || c == '\t') afterKey
                     in case T.uncons afterColon of
                            Just ('{', _) -> fst (extractBracedBlock '{' '}' afterColon)
                            _ -> ""

-- -----------------------------------------------------------------------
-- CoreFn expression parser
-- -----------------------------------------------------------------------

{- | Parse a CoreFn JSON expression into an OrganIR Expr.
Falls back to @ELit (LitInt 0)@ for unrecognised shapes.
-}
parseCoreFnExpr :: Text -> Text -> IR.Expr
parseCoreFnExpr modName_ txt
    | T.null txt = IR.ELit (IR.LitInt 0)
    | otherwise =
        let exprType = extractJsonString "type" txt
         in case exprType of
                "Literal" -> parseLiteral txt
                "Var" -> parseVar modName_ txt
                "App" -> parseApp modName_ txt
                "Abs" -> parseAbs modName_ txt
                "Let" -> parseLet modName_ txt
                "Case" -> parseCaseExpr modName_ txt
                "Constructor" -> parseConstructor modName_ txt
                "Accessor" -> parseAccessor modName_ txt
                "ObjectUpdate" -> parseObjectUpdate modName_ txt
                _ -> IR.ELit (IR.LitInt 0)

-- | Parse a CoreFn Literal expression.
parseLiteral :: Text -> IR.Expr
parseLiteral txt =
    let -- Find the "value" object inside the top-level object
        valueObj = extractValueObject txt
        litType = extractJsonString "literalType" valueObj
     in case litType of
            "IntLiteral" -> IR.ELit (IR.LitInt (extractJsonInt "value" valueObj))
            "NumberLiteral" -> IR.ELit (IR.LitFloat (extractJsonDouble "value" valueObj))
            "StringLiteral" -> IR.ELit (IR.LitString (extractJsonString "value" valueObj))
            "BooleanLiteral" -> IR.ELit (IR.LitBool (extractJsonBool "value" valueObj))
            "CharLiteral" -> IR.ELit (IR.LitString (extractJsonString "value" valueObj))
            "ArrayLiteral" ->
                let items = extractJsonExprArray "value" valueObj
                 in IR.EList (map (parseCoreFnExpr "") items)
            "ObjectLiteral" ->
                -- Object literals: emit as a list of key-value pair applications
                IR.EApp (IR.EVar (IR.Name "objectLiteral" 0)) []
            _ -> IR.ELit (IR.LitInt 0)

{- | Extract the "value" field as a JSON object from an expression node.
CoreFn expressions have the shape {"type": "...", "value": {...}}.
-}
extractValueObject :: Text -> Text
extractValueObject txt =
    -- We need to find "value" that comes after "type", so skip past the type field first
    case findValueField txt of
            "" -> txt -- fallback: search in the whole thing
            v -> v
  where
    findValueField t =
        -- Find "type" first, skip past it, then find "value"
        let typeNeedle = "\"type\""
         in case T.breakOn typeNeedle t of
                (_, rest)
                    | T.null rest -> ""
                    | otherwise ->
                        let afterType = T.drop (T.length typeNeedle) rest
                            -- skip past the type value string
                            afterTypeVal = skipJsonValue (T.dropWhile (\c -> c == ' ' || c == ':' || c == '\n' || c == '\r' || c == '\t') afterType)
                            -- now find "value" key
                            valNeedle = "\"value\""
                         in case T.breakOn valNeedle afterTypeVal of
                                (_, vrest)
                                    | T.null vrest -> ""
                                    | otherwise ->
                                        let afterKey = T.drop (T.length valNeedle) vrest
                                            afterColon = T.dropWhile (\c -> c == ' ' || c == ':' || c == '\n' || c == '\r' || c == '\t') afterKey
                                         in case T.uncons afterColon of
                                                Just ('{', _) -> fst (extractBracedBlock '{' '}' afterColon)
                                                -- For simple values (int, bool, string, array), return from colon onwards
                                                _ -> afterColon

-- | Skip past a single JSON value (string, number, object, array, bool, null).
skipJsonValue :: Text -> Text
skipJsonValue txt = case T.uncons txt of
    Nothing -> txt
    Just ('"', rest) ->
        -- skip string
        let afterStr = dropQuotedString rest
         in afterStr
    Just ('{', _) ->
        -- skip object
        snd (extractBracedBlock '{' '}' txt)
    Just ('[', _) ->
        -- skip array
        snd (extractBracedBlock '[' ']' txt)
    Just (c, _)
        | c == 't' -> T.drop 4 txt -- true
        | c == 'f' -> T.drop 5 txt -- false
        | c == 'n' -> T.drop 4 txt -- null
        | c == '-' || (c >= '0' && c <= '9') ->
            T.dropWhile (\ch -> ch >= '0' && ch <= '9' || ch == '.' || ch == '-' || ch == 'e' || ch == 'E' || ch == '+') txt
        | otherwise -> txt

-- | Drop the contents of a quoted string (we're past the opening quote).
dropQuotedString :: Text -> Text
dropQuotedString txt = case T.uncons txt of
    Nothing -> txt
    Just ('\\', rest) -> case T.uncons rest of
        Just (_, rest') -> dropQuotedString rest'
        Nothing -> rest
    Just ('"', rest) -> rest
    Just (_, rest) -> dropQuotedString rest

-- | Parse a CoreFn Var expression.
parseVar :: Text -> Text -> IR.Expr
parseVar _modName_ txt =
    let valueObj = extractValueObject txt
        ident = extractJsonString "identifier" valueObj
     in if T.null ident
            then IR.ELit (IR.LitInt 0)
            else IR.EVar (IR.Name ident 0)

-- | Parse a CoreFn App expression.
parseApp :: Text -> Text -> IR.Expr
parseApp modName_ txt =
    let valueObj = extractValueObject txt
        absExprTxt = extractInnerExprField "abstraction" valueObj
        argExprTxt = extractInnerExprField "argument" valueObj
        absExpr = parseCoreFnExpr modName_ absExprTxt
        argExpr = parseCoreFnExpr modName_ argExprTxt
     in case absExpr of
            -- Collapse nested single-arg applications: (f a) b -> EApp f [a, b]
            IR.EApp f args -> IR.EApp f (args ++ [argExpr])
            _ -> IR.EApp absExpr [argExpr]

-- | Parse a CoreFn Abs (lambda) expression.
parseAbs :: Text -> Text -> IR.Expr
parseAbs modName_ txt =
    let valueObj = extractValueObject txt
        arg = extractJsonString "argument" valueObj
        bodyTxt = extractInnerExprField "body" valueObj
        bodyExpr = parseCoreFnExpr modName_ bodyTxt
     in case bodyExpr of
            -- Collapse nested lambdas: \x -> \y -> e  =>  ELam [x, y] e
            IR.ELam params inner ->
                IR.ELam (IR.LamParam (IR.Name arg 0) Nothing : params) inner
            _ ->
                IR.ELam [IR.LamParam (IR.Name arg 0) Nothing] bodyExpr

-- | Parse a CoreFn Let expression.
parseLet :: Text -> Text -> IR.Expr
parseLet modName_ txt =
    let valueObj = extractValueObject txt
        bindsArr = extractJsonArray "binds" valueObj
        bodyTxt = extractInnerExprField "expression" valueObj
        bodyExpr = parseCoreFnExpr modName_ bodyTxt
        irBinds = concatMap (parseLetBind modName_) bindsArr
     in IR.ELet irBinds bodyExpr

-- | Parse a single let binding from CoreFn.
parseLetBind :: Text -> Text -> [IR.LetBind]
parseLetBind modName_ bindTxt =
    let bindType = extractJsonString "bindType" bindTxt
     in if bindType == "Rec"
            then
                let bindsArr = extractJsonArray "binds" bindTxt
                 in map (parseSingleBind modName_) bindsArr
            else [parseSingleBind modName_ bindTxt]

-- | Parse a single name-expression bind.
parseSingleBind :: Text -> Text -> IR.LetBind
parseSingleBind modName_ bindTxt =
    let ident = extractJsonString "identifier" bindTxt
        exprTxt = extractExprField bindTxt
        expr = parseCoreFnExpr modName_ exprTxt
     in IR.LetBind (IR.Name ident 0) Nothing expr

-- | Parse a CoreFn Case expression.
parseCaseExpr :: Text -> Text -> IR.Expr
parseCaseExpr modName_ txt =
    let valueObj = extractValueObject txt
        scrutinees = extractJsonExprArray "caseExpressions" valueObj
        alternatives = extractJsonArray "caseAlternatives" valueObj
        -- For now, we take the first scrutinee (PureScript Case usually has one)
        scrutExpr = case scrutinees of
            (s : _) -> parseCoreFnExpr modName_ s
            [] -> IR.ELit (IR.LitInt 0)
        branches = map (parseCaseAlt modName_) alternatives
     in IR.ECase scrutExpr branches

-- | Parse a case alternative.
parseCaseAlt :: Text -> Text -> IR.Branch
parseCaseAlt modName_ altTxt =
    let -- Extract the body expression (either "expression" or "isGuarded" alternatives)
        isGuarded = extractJsonBool "isGuarded" altTxt
        bodyExpr
            | isGuarded =
                -- Guarded case: fall back to first guard body or placeholder
                IR.ELit (IR.LitInt 0)
            | otherwise =
                let exprTxt = extractInnerExprField "expression" altTxt
                 in parseCoreFnExpr modName_ exprTxt
        -- Extract patterns (array of binders)
        binders = extractJsonArray "binders" altTxt
        pat = case binders of
            (b : _) -> parseBinder b
            [] -> IR.PatWild
     in IR.Branch pat bodyExpr

-- | Parse a CoreFn binder (pattern).
parseBinder :: Text -> IR.Pat
parseBinder txt =
    let binderType = extractJsonString "binderType" txt
     in case binderType of
            "NullBinder" -> IR.PatWild
            "VarBinder" ->
                let ident = extractJsonString "identifier" txt
                 in IR.PatVar (IR.Name ident 0) Nothing
            "LiteralBinder" ->
                let valueObj = extractValueObject txt
                    litType = extractJsonString "literalType" valueObj
                 in case litType of
                        "IntLiteral" -> IR.PatLit (IR.LitInt (extractJsonInt "value" valueObj))
                        "StringLiteral" -> IR.PatLit (IR.LitString (extractJsonString "value" valueObj))
                        "BooleanLiteral" -> IR.PatLit (IR.LitBool (extractJsonBool "value" valueObj))
                        "NumberLiteral" -> IR.PatLit (IR.LitFloat (extractJsonDouble "value" valueObj))
                        "CharLiteral" -> IR.PatLit (IR.LitString (extractJsonString "value" valueObj))
                        _ -> IR.PatWild
            "ConstructorBinder" ->
                let ctorName = extractJsonString "constructorName" txt
                    binderArgs = extractJsonArray "binders" txt
                    patBinders = map binderToPatBinder binderArgs
                 in IR.PatCon (IR.QName "" (IR.Name ctorName 0)) patBinders
            "NamedBinder" ->
                let ident = extractJsonString "identifier" txt
                 in IR.PatVar (IR.Name ident 0) Nothing
            _ -> IR.PatWild

-- | Convert a binder JSON to a PatBinder (for constructor args).
binderToPatBinder :: Text -> IR.PatBinder
binderToPatBinder txt =
    let binderType = extractJsonString "binderType" txt
        ident = case binderType of
            "VarBinder" -> extractJsonString "identifier" txt
            "NamedBinder" -> extractJsonString "identifier" txt
            _ -> "_"
     in IR.PatBinder (IR.Name ident 0) Nothing

-- | Parse a CoreFn Constructor expression.
parseConstructor :: Text -> Text -> IR.Expr
parseConstructor modName_ txt =
    let valueObj = extractValueObject txt
        _typeName = extractJsonString "typeName" valueObj
        ctorName = extractJsonString "constructorName" valueObj
        _fieldNames = extractJsonStringArray "fieldNames" valueObj
     in IR.ECon (IR.QName modName_ (IR.Name ctorName 0)) []

-- | Parse a CoreFn Accessor expression (record field access).
parseAccessor :: Text -> Text -> IR.Expr
parseAccessor modName_ txt =
    let valueObj = extractValueObject txt
        fieldName = extractJsonString "fieldName" valueObj
        exprTxt = extractInnerExprField "expression" valueObj
        innerExpr = parseCoreFnExpr modName_ exprTxt
     in IR.EApp (IR.EVar (IR.Name "accessor" 0)) [IR.ELit (IR.LitString fieldName), innerExpr]

-- | Parse a CoreFn ObjectUpdate expression.
parseObjectUpdate :: Text -> Text -> IR.Expr
parseObjectUpdate modName_ txt =
    let valueObj = extractValueObject txt
        exprTxt = extractInnerExprField "expression" valueObj
        innerExpr = parseCoreFnExpr modName_ exprTxt
     in IR.EApp (IR.EVar (IR.Name "objectUpdate" 0)) [innerExpr]

-- -----------------------------------------------------------------------
-- Additional JSON extraction helpers
-- -----------------------------------------------------------------------

{- | Extract a named field that contains a JSON expression object.
Returns the braced JSON text of the expression.
-}
extractInnerExprField :: Text -> Text -> Text
extractInnerExprField key txt =
    let needle = "\"" <> key <> "\""
     in case T.breakOn needle txt of
            (_, rest)
                | T.null rest -> ""
                | otherwise ->
                    let afterKey = T.drop (T.length needle) rest
                        afterColon = T.dropWhile (\c -> c == ' ' || c == ':' || c == '\n' || c == '\r' || c == '\t') afterKey
                     in case T.uncons afterColon of
                            Just ('{', _) -> fst (extractBracedBlock '{' '}' afterColon)
                            _ -> ""

-- | Extract a JSON integer value by key.
extractJsonInt :: Text -> Text -> Integer
extractJsonInt key txt =
    let needle = "\"" <> key <> "\""
     in case T.breakOn needle txt of
            (_, rest)
                | T.null rest -> 0
                | otherwise ->
                    let afterKey = T.drop (T.length needle) rest
                        afterColon = T.dropWhile (\c -> c == ' ' || c == ':' || c == '\n' || c == '\r' || c == '\t') afterKey
                        numText = T.takeWhile (\c -> c >= '0' && c <= '9' || c == '-') afterColon
                     in case reads (T.unpack numText) of
                            ((n, _) : _) -> n
                            _ -> 0

-- | Extract a JSON floating point value by key.
extractJsonDouble :: Text -> Text -> Double
extractJsonDouble key txt =
    let needle = "\"" <> key <> "\""
     in case T.breakOn needle txt of
            (_, rest)
                | T.null rest -> 0.0
                | otherwise ->
                    let afterKey = T.drop (T.length needle) rest
                        afterColon = T.dropWhile (\c -> c == ' ' || c == ':' || c == '\n' || c == '\r' || c == '\t') afterKey
                        numText = T.takeWhile (\c -> c >= '0' && c <= '9' || c == '-' || c == '.' || c == 'e' || c == 'E' || c == '+') afterColon
                     in case reads (T.unpack numText) of
                            ((n, _) : _) -> n
                            _ -> 0.0

-- | Extract a JSON boolean value by key.
extractJsonBool :: Text -> Text -> Bool
extractJsonBool key txt =
    let needle = "\"" <> key <> "\""
     in case T.breakOn needle txt of
            (_, rest)
                | T.null rest -> False
                | otherwise ->
                    let afterKey = T.drop (T.length needle) rest
                        afterColon = T.dropWhile (\c -> c == ' ' || c == ':' || c == '\n' || c == '\r' || c == '\t') afterKey
                     in T.isPrefixOf "true" afterColon

{- | Extract a JSON array of objects/expressions by key.
Returns a list of the raw JSON text for each element.
-}
extractJsonArray :: Text -> Text -> [Text]
extractJsonArray key txt =
    let needle = "\"" <> key <> "\""
     in case T.breakOn needle txt of
            (_, rest)
                | T.null rest -> []
                | otherwise ->
                    let afterKey = T.drop (T.length needle) rest
                        afterColon = T.dropWhile (\c -> c == ' ' || c == ':' || c == '\n' || c == '\r' || c == '\t') afterKey
                     in case T.uncons afterColon of
                            Just ('[', arrBody) -> parseObjectArray arrBody
                            _ -> []

{- | Extract a JSON array of expression objects by key.
Same as extractJsonArray but handles the case where elements are objects.
-}
extractJsonExprArray :: Text -> Text -> [Text]
extractJsonExprArray = extractJsonArray

-- | Parse an array body containing JSON objects, returning each object as text.
parseObjectArray :: Text -> [Text]
parseObjectArray = go . T.strip
  where
    go txt
        | T.null txt = []
        | otherwise = case T.uncons txt of
            Nothing -> []
            Just (']', _) -> []
            Just ('{', _) ->
                let (obj, afterObj) = extractBracedBlock '{' '}' txt
                    rest = T.dropWhile (\c -> c == ' ' || c == ',' || c == '\n' || c == '\r' || c == '\t') afterObj
                 in obj : go rest
            Just ('[', _) ->
                let (arr, afterArr) = extractBracedBlock '[' ']' txt
                    rest = T.dropWhile (\c -> c == ' ' || c == ',' || c == '\n' || c == '\r' || c == '\t') afterArr
                 in arr : go rest
            Just (_, rest) -> go rest

-- -----------------------------------------------------------------------
-- OrganIR emission (via organ-ir library)
-- -----------------------------------------------------------------------

-- | Convert a CoreFn declaration to an OrganIR Definition.
declToIR :: Text -> Int -> CoreFnDecl -> IR.Definition
declToIR modName_ uniq decl =
    let arity = declArity decl
        qname = IR.QName modName_ (IR.Name (declIdent decl) uniq)
        ty
            | arity > 0 =
                IR.TFn
                    (replicate arity (IR.FnArg (Just IR.Many) IR.TAny))
                    IR.pureEffect
                    IR.TAny
            | otherwise = IR.TAny
        expr = parseCoreFnExpr modName_ (declExprText decl)
        sort = if arity > 0 then IR.SFun else IR.SVal
     in IR.Definition qname ty expr sort IR.Public arity
