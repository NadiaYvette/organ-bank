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
    , declIsRec :: Bool
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
                                let trimmed = T.dropWhile (\c -> c == ' ' || c == '"' || c == '\n' || c == '\r' || c == '\t') arrRest
                                 in case T.breakOn "\"" trimmed of
                                        (modPart, _) ->
                                            T.intercalate "." $
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
                    Just ('{', objRest) ->
                        let (obj, afterObj) = extractBracedBlock '{' '}' stripped
                            bindType = extractJsonString "bindType" obj
                            decls
                                | bindType == "NonRec" =
                                    let ident = extractJsonString "identifier" obj
                                        arity = countAbsNodes obj
                                     in [CoreFnDecl ident arity False]
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
                            rest = T.dropWhile (\c -> c == ' ' || c == ',' || c == '\n' || c == '\r' || c == '\t') afterObj
                         in CoreFnDecl ident arity True : go rest
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
        expr
            | arity > 0 =
                IR.ELam
                    ( map
                        (\i -> IR.LamParam (IR.Name ("$" <> T.pack (show i)) 0) Nothing)
                        [1 .. arity]
                    )
                    (IR.ELit (IR.LitInt 0))
            | otherwise = IR.ELit (IR.LitInt 0)
        sort = if arity > 0 then IR.SFun else IR.SVal
     in IR.Definition qname ty expr sort IR.Public arity
