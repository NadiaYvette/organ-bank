-- | Extract GCC GIMPLE from Ada source via GNAT and emit OrganIR JSON.
module OrganBank.AdaShim (extractOrganIR) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, listDirectory, makeAbsolute,
                         getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>), takeBaseName)
import Control.Exception (bracket, catch, SomeException)

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
  absInput <- makeAbsolute inputPath
  let tmpDir = "/tmp/organ-bank-ada-" ++ show (hash inputPath)
  createDirectoryIfMissing True tmpDir
  -- GNAT puts dump files in cwd, so we cd to tmpDir
  result <- bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory tmpDir
    (ec, _out, err) <- readProcessWithExitCode "gnat"
      ["compile", "-fdump-tree-gimple", "-c", "-gnatc", absInput] ""
    case ec of
      ExitFailure _ -> return $ Left $ "gnat compile failed: " ++ err
      ExitSuccess -> do
        dumpFile <- findGimpleFile tmpDir
        case dumpFile of
          Nothing -> return $ Left "No GIMPLE dump file produced"
          Just df -> do
            content <- TIO.readFile df
            let defs = parseGimple content
                -- Filter out Ada elaboration routines
                userDefs = filter (not . isElaboration) defs
                modName = takeBaseName inputPath
            return $ Right $ emitOrganIR modName inputPath userDefs
  cleanup tmpDir
  return result

hash :: String -> Int
hash = foldl (\h c -> h * 31 + fromEnum c) 0

cleanup :: FilePath -> IO ()
cleanup dir = removeDirectoryRecursive dir `catch` (\(_ :: SomeException) -> return ())

findGimpleFile :: FilePath -> IO (Maybe FilePath)
findGimpleFile dir = do
  files <- listDirectory dir
  let gimples = filter (\f -> ".gimple" `isSuffixOf'` f) files
  case gimples of
    (f:_) -> return $ Just (dir </> f)
    []    -> return Nothing

isSuffixOf' :: String -> String -> Bool
isSuffixOf' suf s = reverse suf == take (length suf) (reverse s)

isElaboration :: GimpleFunc -> Bool
isElaboration f =
  any (`T.isSuffixOf` gfName f) ["___elabb", "___elabs", "_E", "___finalizer"]

data GimpleFunc = GimpleFunc
  { gfName   :: Text
  , gfBlocks :: [(Text, [Text])]
  } deriving (Show)

parseGimple :: Text -> [GimpleFunc]
parseGimple content = parseFunctions (T.lines content)

parseFunctions :: [Text] -> [GimpleFunc]
parseFunctions [] = []
parseFunctions (l:ls)
  | ";; Function " `T.isPrefixOf` l || not (T.null l) && not (" " `T.isPrefixOf` l) && "(" `T.isInfixOf` l && "{" `T.isInfixOf` (T.pack (unlines' (take 3 (map T.unpack (l:ls))))) =
    let name = extractFuncName l
        (bodyLines, rest) = collectBody ls 0
        blocks = parseGimpleBlocks bodyLines
    in GimpleFunc name blocks : parseFunctions rest
  | otherwise = parseFunctions ls
  where unlines' = unwords

extractFuncName :: Text -> Text
extractFuncName l
  | ";; Function " `T.isPrefixOf` l =
    let afterPrefix = T.drop 14 l
    in T.takeWhile (\c -> c /= ' ' && c /= '(') afterPrefix
  | otherwise = T.takeWhile (\c -> c /= ' ' && c /= '(') (T.strip l)

collectBody :: [Text] -> Int -> ([Text], [Text])
collectBody [] _ = ([], [])
collectBody (l:ls) depth
  | "{" `T.isSuffixOf` T.strip l =
    if depth == 0 then let (b, r) = collectBody ls 1 in (b, r)
    else let (b, r) = collectBody ls (depth + 1) in (l:b, r)
  | "}" `T.isSuffixOf` T.strip l =
    if depth <= 1 then ([], ls)
    else let (b, r) = collectBody ls (depth - 1) in (l:b, r)
  | depth > 0 = let (b, r) = collectBody ls depth in (l:b, r)
  | otherwise = collectBody ls depth

parseGimpleBlocks :: [Text] -> [(Text, [Text])]
parseGimpleBlocks = go "entry" []
  where
    go lbl acc [] = if null acc then [] else [(lbl, reverse acc)]
    go lbl acc (l:ls)
      | "  <bb " `T.isPrefixOf` l || "<bb " `T.isPrefixOf` T.strip l =
        let cleaned = T.strip l
            num = T.takeWhile (/= '>') (T.drop 4 cleaned)
            prev = if null acc then [] else [(lbl, reverse acc)]
        in prev ++ go (T.concat ["bb_", num]) [] ls
      | "  <D." `T.isPrefixOf` l =
        let cleaned = T.strip l
            lname = T.takeWhile (/= '>') (T.drop 1 cleaned)
            prev = if null acc then [] else [(lbl, reverse acc)]
        in prev ++ go lname [] ls
      | otherwise =
        let trimmed = T.strip l
        in if T.null trimmed then go lbl acc ls
           else go lbl (trimmed : acc) ls

emitOrganIR :: String -> FilePath -> [GimpleFunc] -> Text
emitOrganIR modName srcFile defs = T.concat
  [ "{\n  \"source_language\": \"ada\",\n"
  , "  \"module_name\": ", jsonStr (T.pack modName), ",\n"
  , "  \"source_file\": ", jsonStr (T.pack srcFile), ",\n"
  , "  \"compiler_version\": \"gnat-gimple\",\n"
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
  where esc '"' = "\\\""; esc '\\' = "\\\\"; esc '\n' = "\\n"; esc '\t' = "\\t"; esc c = T.singleton c
