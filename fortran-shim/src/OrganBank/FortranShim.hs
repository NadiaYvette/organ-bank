-- | Extract GCC GIMPLE from Fortran source via gfortran and emit OrganIR JSON.
module OrganBank.FortranShim (extractOrganIR) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, listDirectory)
import Control.Exception (catch, SomeException)
import System.FilePath ((</>), takeBaseName)

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
  let tmpDir = "/tmp/organ-bank-fortran-" ++ show (hash inputPath)
  createDirectoryIfMissing True tmpDir
  (ec, _out, err) <- readProcessWithExitCode "gfortran"
    ["-fdump-tree-optimized", "-O2", "-c", inputPath, "-o", tmpDir </> "out.o"] ""
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
              json = emitOrganIR modName inputPath defs
          cleanup tmpDir
          return $ Right json

hash :: String -> Int
hash = foldl (\h c -> h * 31 + fromEnum c) 0

cleanup :: FilePath -> IO ()
cleanup dir = removeDirectoryRecursive dir `catch` (\(_ :: SomeException) -> return ())


findDumpDir :: FilePath -> FilePath -> IO FilePath
findDumpDir inputPath _tmpDir = do
  -- gfortran puts dump files next to the source file
  let srcDir = case reverse (dropWhile (/= '/') (reverse inputPath)) of
                 "" -> "."
                 d  -> init d  -- drop trailing /
  return srcDir

findOptimizedFile :: FilePath -> IO (Maybe FilePath)
findOptimizedFile dir = do
  files <- listDirectory dir
  let opts = filter (\f -> ".optimized" `isSuffix` f) files
  case opts of
    (f:_) -> return $ Just (dir </> f)
    []    -> return Nothing
  where isSuffix suf s = suf == reverse (take (length suf) (reverse s))

data GimpleFunc = GimpleFunc
  { gfName   :: Text
  , gfParams :: Text
  , gfBlocks :: [(Text, [Text])]
  } deriving (Show)

parseGimple :: Text -> [GimpleFunc]
parseGimple content = parseFunctions (T.lines content)

parseFunctions :: [Text] -> [GimpleFunc]
parseFunctions [] = []
parseFunctions (l:ls)
  | ";; Function " `T.isPrefixOf` l =
    let name = extractFuncName l
        (header, bodyLs) = case ls of
          (h:rest) -> (h, rest)
          [] -> ("", [])
        (bodyLines, rest) = collectGimpleBody bodyLs 0
        params = T.strip header
        blocks = parseGimpleBlocks bodyLines
    in GimpleFunc name params blocks : parseFunctions rest
  | otherwise = parseFunctions ls

extractFuncName :: Text -> Text
extractFuncName l =
  let afterPrefix = T.drop 14 l  -- drop ";; Function "
      name = T.takeWhile (\c -> c /= ' ' && c /= '(') afterPrefix
  in name

collectGimpleBody :: [Text] -> Int -> ([Text], [Text])
collectGimpleBody [] _ = ([], [])
collectGimpleBody (l:ls) depth
  | "{" `T.isSuffixOf` T.strip l =
    let (b, r) = collectGimpleBody ls (depth + 1) in (l:b, r)
  | "}" `T.isSuffixOf` T.strip l =
    if depth <= 1 then ([], ls)
    else let (b, r) = collectGimpleBody ls (depth - 1) in (l:b, r)
  | otherwise = let (b, r) = collectGimpleBody ls depth in (l:b, r)

parseGimpleBlocks :: [Text] -> [(Text, [Text])]
parseGimpleBlocks = go "entry" []
  where
    go lbl acc [] = if null acc then [] else [(lbl, reverse acc)]
    go lbl acc (l:ls)
      | "  <bb " `T.isPrefixOf` l =
        let newLbl = T.strip (T.takeWhile (/= '>') (T.drop 5 l))
            prev = if null acc then [] else [(lbl, reverse acc)]
        in prev ++ go (T.concat ["bb_", newLbl]) [] ls
      | otherwise =
        let trimmed = T.strip l
        in if T.null trimmed
           then go lbl acc ls
           else go lbl (trimmed : acc) ls

emitOrganIR :: String -> FilePath -> [GimpleFunc] -> Text
emitOrganIR modName srcFile defs = T.concat
  [ "{\n  \"source_language\": \"fortran\",\n"
  , "  \"module_name\": ", jsonStr (T.pack modName), ",\n"
  , "  \"source_file\": ", jsonStr (T.pack srcFile), ",\n"
  , "  \"compiler_version\": \"gfortran-gimple\",\n"
  , "  \"definitions\": [\n"
  , T.intercalate ",\n" (map emitDef defs)
  , "\n  ]\n}\n"
  ]

emitDef :: GimpleFunc -> Text
emitDef f = T.concat
  [ "    {\n"
  , "      \"name\": {\"module\": \"\", \"text\": ", jsonStr (gfName f), ", \"unique\": 0},\n"
  , "      \"type\": {\"tag\": \"any\"},\n"
  , "      \"expr\": {\"tag\": \"ssa_body\", \"blocks\": [\n"
  , T.intercalate ",\n" (map emitBlock (gfBlocks f))
  , "\n      ]},\n"
  , "      \"sort\": \"fun\",\n      \"visibility\": \"public\",\n"
  , "      \"arity\": 0\n    }"
  ]

emitBlock :: (Text, [Text]) -> Text
emitBlock (lbl, stmts) = T.concat
  [ "        {\"label\": ", jsonStr lbl, ", \"stmts\": [\n"
  , T.intercalate ",\n" (map (\s -> T.concat ["          ", jsonStr s]) stmts)
  , "\n        ]}"
  ]

jsonStr :: Text -> Text
jsonStr t = T.concat ["\"", T.concatMap esc t, "\""]
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc c    = T.singleton c
