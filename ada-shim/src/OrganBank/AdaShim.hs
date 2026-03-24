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

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
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
                        pure $ Right $ emitOrganIR modName inputPath userDefs
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
    , gfBlocks :: [(Text, [Text])]
    }
    deriving (Show)

parseGimple :: Text -> [GimpleFunc]
parseGimple content = parseFunctions (T.lines content)

parseFunctions :: [Text] -> [GimpleFunc]
parseFunctions [] = []
parseFunctions (l : ls)
    | ";; Function " `T.isPrefixOf` l
        || not (T.null l)
            && not (" " `T.isPrefixOf` l)
            && "(" `T.isInfixOf` l
            && "{" `T.isInfixOf` T.pack (unlines' (take 3 (map T.unpack (l : ls)))) =
        let name = extractFuncName l
            (bodyLines, rest) = collectBody ls 0
            blocks = parseGimpleBlocks bodyLines
         in GimpleFunc name blocks : parseFunctions rest
    | otherwise = parseFunctions ls
  where
    unlines' = unwords

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

-- | Emit OrganIR JSON using the organ-ir library.
emitOrganIR :: String -> FilePath -> [GimpleFunc] -> Text
emitOrganIR modName srcFile defs =
    renderOrganIR $
        IR.simpleOrganIR IR.LAda "ada-shim-0.1" (T.pack modName) srcFile (map funcToIR defs)

funcToIR :: GimpleFunc -> IR.Definition
funcToIR f =
    IR.Definition
        { IR.defName = IR.localName (gfName f)
        , IR.defType = IR.TAny
        , IR.defExpr = IR.EApp (IR.eVar "gimple_body") (map blockToIR (gfBlocks f))
        , IR.defSort = IR.SFun
        , IR.defVisibility = IR.Public
        , IR.defArity = 0
        }

blockToIR :: (Text, [Text]) -> IR.Expr
blockToIR (lbl, stmts) = IR.ETuple [IR.eString lbl, IR.EList (map IR.eString stmts)]
