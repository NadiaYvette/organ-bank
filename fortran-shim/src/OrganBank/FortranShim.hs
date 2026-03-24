-- | Extract GCC GIMPLE from Fortran source via gfortran and emit OrganIR JSON.
module OrganBank.FortranShim (extractOrganIR) where

import Control.Exception (SomeException, catch)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Directory (createDirectoryIfMissing, listDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, (</>))
import System.Process (readProcessWithExitCode)

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
    let tmpDir = "/tmp/organ-bank-fortran-" ++ show (hash inputPath)
    createDirectoryIfMissing True tmpDir
    (ec, _out, err) <-
        readProcessWithExitCode
            "gfortran"
            ["-fdump-tree-optimized", "-O2", "-c", inputPath, "-o", tmpDir </> "out.o"]
            ""
    case ec of
        ExitFailure _ -> do
            cleanup tmpDir
            return $ Left $ "gfortran failed: " ++ err
        ExitSuccess -> do
            -- Find the .optimized dump file (gfortran puts it next to source)
            srcDir <- findDumpDir inputPath tmpDir
            dumpFile <- findOptimizedFile srcDir
            case dumpFile of
                Nothing -> do
                    cleanup tmpDir
                    return $ Left "No GIMPLE dump file produced"
                Just df -> do
                    content <- TIO.readFile df
                    let defs = parseGimple content
                        modName = takeBaseName inputPath
                        ir = IR.simpleOrganIR IR.LFortran "fortran-shim-0.1" (T.pack modName) inputPath (map funcToIR defs)
                    cleanup tmpDir
                    return $ Right (renderOrganIR ir)

hash :: String -> Int
hash = foldl (\h c -> h * 31 + fromEnum c) 0

cleanup :: FilePath -> IO ()
cleanup dir = removeDirectoryRecursive dir `catch` (\(_ :: SomeException) -> return ())

findDumpDir :: FilePath -> FilePath -> IO FilePath
findDumpDir inputPath _tmpDir = do
    -- gfortran puts dump files next to the source file
    let srcDir = case reverse (dropWhile (/= '/') (reverse inputPath)) of
            "" -> "."
            d -> init d -- drop trailing /
    return srcDir

findOptimizedFile :: FilePath -> IO (Maybe FilePath)
findOptimizedFile dir = do
    files <- listDirectory dir
    let opts = filter (\f -> ".optimized" `isSuffix` f) files
    case opts of
        (f : _) -> return $ Just (dir </> f)
        [] -> return Nothing
  where
    isSuffix suf s = suf == reverse (take (length suf) (reverse s))

data GimpleFunc = GimpleFunc
    { gfName :: Text
    , gfParams :: Text
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
            (header, bodyLs) = case ls of
                (h : rest) -> (h, rest)
                [] -> ("", [])
            (bodyLines, rest) = collectGimpleBody bodyLs 0
            params = T.strip header
            blocks = parseGimpleBlocks bodyLines
         in GimpleFunc name params blocks : parseFunctions rest
    | otherwise = parseFunctions ls

extractFuncName :: Text -> Text
extractFuncName l =
    let afterPrefix = T.drop 14 l -- drop ";; Function "
        name = T.takeWhile (\c -> c /= ' ' && c /= '(') afterPrefix
     in name

collectGimpleBody :: [Text] -> Int -> ([Text], [Text])
collectGimpleBody [] _ = ([], [])
collectGimpleBody (l : ls) depth
    | "{" `T.isSuffixOf` T.strip l =
        let (b, r) = collectGimpleBody ls (depth + 1) in (l : b, r)
    | "}" `T.isSuffixOf` T.strip l =
        if depth <= 1
            then ([], ls)
            else let (b, r) = collectGimpleBody ls (depth - 1) in (l : b, r)
    | otherwise = let (b, r) = collectGimpleBody ls depth in (l : b, r)

parseGimpleBlocks :: [Text] -> [(Text, [Text])]
parseGimpleBlocks = go "entry" []
  where
    go lbl acc [] = [(lbl, reverse acc) | not (null acc)]
    go lbl acc (l : ls)
        | "  <bb " `T.isPrefixOf` l =
            let newLbl = T.strip (T.takeWhile (/= '>') (T.drop 5 l))
                prev = [(lbl, reverse acc) | not (null acc)]
             in prev ++ go (T.concat ["bb_", newLbl]) [] ls
        | otherwise =
            let trimmed = T.strip l
             in if T.null trimmed
                    then go lbl acc ls
                    else go lbl (trimmed : acc) ls

-- | Convert a parsed GIMPLE function to an OrganIR definition.
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

-- | Convert a GIMPLE basic block to an OrganIR expression.
blockToIR :: (Text, [Text]) -> IR.Expr
blockToIR (lbl, stmts) = IR.ETuple [IR.eString lbl, IR.EList (map IR.eString stmts)]
