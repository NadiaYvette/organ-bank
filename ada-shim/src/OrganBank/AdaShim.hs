-- | Extract GCC GIMPLE from Ada source via GNAT and emit OrganIR JSON.
module OrganBank.AdaShim (extractOrganIR) where

import Control.Exception (SomeException, bracket, catch)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Directory (
    createDirectoryIfMissing,
    getCurrentDirectory,
    listDirectory,
    makeAbsolute,
    removeDirectoryRecursive,
    setCurrentDirectory,
 )
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, (</>))
import System.Process (readProcessWithExitCode)

-- | Detect GNAT version by running @gnat --version@.
detectCompilerVersion :: IO Text
detectCompilerVersion = do
    (ec, out, _) <- readProcessWithExitCode "gnat" ["--version"] ""
    pure $ case ec of
        ExitSuccess | (l : _) <- lines out -> "ada-shim-0.1 (" <> T.strip (T.pack l) <> ")"
        _ -> "ada-shim-0.1"
  `catch` \(_ :: SomeException) -> pure "ada-shim-0.1"

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
    shimVer <- detectCompilerVersion
    absInput <- makeAbsolute inputPath
    let tmpDir = "/tmp/organ-bank-ada-" ++ show (hash inputPath)
    createDirectoryIfMissing True tmpDir
    -- GNAT puts dump files in cwd, so we cd to tmpDir
    result <- bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
        setCurrentDirectory tmpDir
        (ec, _out, err) <-
            readProcessWithExitCode
                "gnat"
                ["compile", "-fdump-tree-gimple", "-c", "-gnatc", absInput]
                ""
        case ec of
            ExitFailure _ -> pure $ Left $ "gnat compile failed: " ++ err
            ExitSuccess -> do
                dumpFile <- findGimpleFile tmpDir
                case dumpFile of
                    Nothing -> pure $ Left "No GIMPLE dump file produced"
                    Just df -> do
                        content <- TIO.readFile df
                        let defs = parseGimple content
                            -- Filter out Ada elaboration routines
                            userDefs = filter (not . isElaboration) defs
                            modName = takeBaseName inputPath
                        pure $ Right $ emitOrganIR shimVer modName inputPath userDefs
    cleanup tmpDir
    pure result

hash :: String -> Int
hash = foldl (\h c -> h * 31 + fromEnum c) 0

cleanup :: FilePath -> IO ()
cleanup dir = removeDirectoryRecursive dir `catch` (\(_ :: SomeException) -> pure ())

findGimpleFile :: FilePath -> IO (Maybe FilePath)
findGimpleFile dir = do
    files <- listDirectory dir
    let gimples = filter (\f -> ".gimple" `isSuffixOf'` f) files
    case gimples of
        (f : _) -> pure $ Just (dir </> f)
        [] -> pure Nothing

isSuffixOf' :: String -> String -> Bool
isSuffixOf' suf s = reverse suf == take (length suf) (reverse s)

isElaboration :: GimpleFunc -> Bool
isElaboration f =
    any (`T.isSuffixOf` gfName f) ["___elabb", "___elabs", "_E", "___finalizer"]

data GimpleFunc = GimpleFunc
    { gfName :: Text
    , gfRetTy :: Text
    , gfParamTys :: [(Text, Text)] -- (type, name)
    , gfBlocks :: [(Text, [Text])]
    }
    deriving (Show)

parseGimple :: Text -> [GimpleFunc]
parseGimple content = parseFunctions (T.lines content)

parseFunctions :: [Text] -> [GimpleFunc]
parseFunctions [] = []
parseFunctions (l : ls)
    | ";; Function " `T.isPrefixOf` l =
        let name = extractFuncName l
            -- Collect signature lines until we hit '{'
            (sigLines, bodyLs) = span (\x -> not ("{" `T.isSuffixOf` T.strip x)) ls
            bodyLs' = drop 1 bodyLs -- skip the '{' line
            sig = T.strip (T.unwords (map T.strip sigLines))
            (retTy, paramTys) = parseGimpleSig sig
            (bodyLines, rest) = collectBody bodyLs' 1
            blocks = parseGimpleBlocks bodyLines
         in GimpleFunc name retTy paramTys blocks : parseFunctions rest
    | not (T.null l)
        && not (" " `T.isPrefixOf` l)
        && "(" `T.isInfixOf` l
        && "{" `T.isInfixOf` T.pack (unwords (take 3 (map T.unpack (l : ls)))) =
        let name = extractFuncName l
            -- Signature is on this line and possibly the next
            allSigLines = l : takeWhile (\x -> not ("{" `T.isSuffixOf` T.strip x)) ls
            afterSig = drop (length allSigLines - 1) ls
            bodyLs = dropWhile (\x -> not ("{" `T.isSuffixOf` T.strip x)) afterSig
            bodyLs' = case bodyLs of
                (_ : r) -> r
                [] -> []
            sig = T.strip (T.unwords (map T.strip allSigLines))
            (retTy, paramTys) = parseGimpleSig sig
            (bodyLines, rest) = collectBody bodyLs' 1
            blocks = parseGimpleBlocks bodyLines
         in GimpleFunc name retTy paramTys blocks : parseFunctions rest
    | otherwise = parseFunctions ls

extractFuncName :: Text -> Text
extractFuncName l
    | ";; Function " `T.isPrefixOf` l =
        let afterPrefix = T.drop 14 l
         in T.takeWhile (\c -> c /= ' ' && c /= '(') afterPrefix
    | otherwise = T.takeWhile (\c -> c /= ' ' && c /= '(') (T.strip l)

collectBody :: [Text] -> Int -> ([Text], [Text])
collectBody [] _ = ([], [])
collectBody (l : ls) depth
    | "{" `T.isSuffixOf` T.strip l =
        if depth == 0
            then let (b, r) = collectBody ls 1 in (b, r)
            else let (b, r) = collectBody ls (depth + 1) in (l : b, r)
    | "}" `T.isSuffixOf` T.strip l =
        if depth <= 1
            then ([], ls)
            else let (b, r) = collectBody ls (depth - 1) in (l : b, r)
    | depth > 0 = let (b, r) = collectBody ls depth in (l : b, r)
    | otherwise = collectBody ls depth

parseGimpleBlocks :: [Text] -> [(Text, [Text])]
parseGimpleBlocks = go "entry" []
  where
    go lbl acc [] = [(lbl, reverse acc) | not (null acc)]
    go lbl acc (l : ls)
        | "  <bb " `T.isPrefixOf` l || "<bb " `T.isPrefixOf` T.strip l =
            let cleaned = T.strip l
                num = T.takeWhile (/= '>') (T.drop 4 cleaned)
                prev = [(lbl, reverse acc) | not (null acc)]
             in prev ++ go (T.concat ["bb_", num]) [] ls
        | "  <D." `T.isPrefixOf` l =
            let cleaned = T.strip l
                lname = T.takeWhile (/= '>') (T.drop 1 cleaned)
                prev = [(lbl, reverse acc) | not (null acc)]
             in prev ++ go lname [] ls
        | otherwise =
            let trimmed = T.strip l
             in if T.null trimmed then go lbl acc ls else go lbl (trimmed : acc) ls

-- | Parse a GIMPLE C-like function signature.
--   e.g. "int factorial (int n, int m)" or "void proc (integer x)"
--   Returns (returnType, [(paramType, paramName)])
parseGimpleSig :: Text -> (Text, [(Text, Text)])
parseGimpleSig sig
    | T.null sig = ("void", [])
    | otherwise =
        let (beforeParams, rest) = T.breakOn " (" sig
            ws = T.words beforeParams
            (retTyWords, _nameWords) = case ws of
                [] -> ([], [])
                _ -> (init ws, [last ws])
            retTy = if null retTyWords then "void" else T.unwords retTyWords
            paramStr =
                if T.null rest
                    then ""
                    else T.takeWhile (/= ')') (T.drop 2 rest)
            params = parseGimpleParams paramStr
         in (retTy, params)

-- | Parse GIMPLE parameter list.
parseGimpleParams :: Text -> [(Text, Text)]
parseGimpleParams t
    | T.null (T.strip t) = []
    | otherwise = concatMap parseOneParam (splitGimpleParams t)
  where
    parseOneParam p =
        let ws = T.words (T.strip p)
         in case ws of
                [] -> []
                [_] -> [(T.strip p, "")]
                _ -> [(T.unwords (init ws), last ws)]

-- | Split params on commas, respecting parentheses.
splitGimpleParams :: Text -> [Text]
splitGimpleParams = go (0 :: Int) "" . T.unpack
  where
    go _ acc [] = [T.pack (reverse acc)]
    go depth acc (c : cs)
        | c == '(' = go (depth + 1) (c : acc) cs
        | c == ')' = go (max 0 (depth - 1)) (c : acc) cs
        | c == ',' && depth == 0 = T.pack (reverse acc) : go 0 "" cs
        | otherwise = go depth (c : acc) cs

-- | Map a GIMPLE type string (Ada/GNAT style) to an OrganIR type.
gimpleTypeToIR :: Text -> IR.Ty
gimpleTypeToIR t = case stripped of
    "void" -> IR.tCon "Void"
    "integer" -> IR.tCon "Int32"
    "long_integer" -> IR.tCon "Int64"
    "short_integer" -> IR.tCon "Int16"
    "short_short_integer" -> IR.tCon "Int8"
    "long_long_integer" -> IR.tCon "Int64"
    "natural" -> IR.tCon "Nat32"
    "positive" -> IR.tCon "Nat32"
    "float" -> IR.tCon "Float32"
    "long_float" -> IR.tCon "Float64"
    "boolean" -> IR.tCon "Bool"
    "character" -> IR.tCon "Char"
    "int" -> IR.tCon "Int32"
    "long int" -> IR.tCon "Int64"
    "short int" -> IR.tCon "Int16"
    "unsigned int" -> IR.tCon "UInt32"
    "long unsigned int" -> IR.tCon "UInt64"
    s
        | "*" `T.isSuffixOf` s -> IR.tCon "Ptr"
        | "access" `T.isPrefixOf` T.toLower s -> IR.tCon "Ptr"
        | otherwise -> IR.tCon s
  where
    stripped = T.toLower (T.strip t)

-- | Emit OrganIR JSON using the organ-ir library.
emitOrganIR :: Text -> String -> FilePath -> [GimpleFunc] -> Text
emitOrganIR shimVer modName srcFile defs =
    renderOrganIR $
        IR.simpleOrganIR IR.LAda shimVer (T.pack modName) srcFile (map funcToIR defs)

funcToIR :: GimpleFunc -> IR.Definition
funcToIR f =
    IR.Definition
        { IR.defName = IR.localName (gfName f)
        , IR.defType =
            IR.TFn
                (map (\(ty, _) -> IR.FnArg Nothing (gimpleTypeToIR ty)) (gfParamTys f))
                IR.pureEffect
                (gimpleTypeToIR (gfRetTy f))
        , IR.defExpr = IR.EApp (IR.eVar "gimple_body") (map blockToIR (gfBlocks f))
        , IR.defSort = IR.SFun
        , IR.defVisibility = IR.Public
        , IR.defArity = length (gfParamTys f)
        }

blockToIR :: (Text, [Text]) -> IR.Expr
blockToIR (lbl, stmts) = IR.ETuple [IR.eString lbl, IR.EList (map IR.eString stmts)]
