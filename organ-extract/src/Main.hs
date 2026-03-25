module Main (main) where

import Data.Char (isSpace)
import Data.IORef
import Data.List (isPrefixOf, nub, sortBy)
import Data.Ord (comparing)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , makeAbsolute
  )
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (takeExtension, takeDirectory, (</>), makeRelative, (-<.>))
import System.IO (hClose, hGetContents', hPutStrLn, stderr)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , waitForProcess
  )

-- | Map file extension to the organ-bank executable name.
extToExe :: String -> Maybe String
extToExe ext = case ext of
  ".hs"   -> Just "ghc-organ"
  ".rs"   -> Just "rustc-organ"
  ".m"    -> Just "mmc-organ"
  ".idr"  -> Just "idris2-organ"
  ".lean" -> Just "lean4-organ"
  ".erl"  -> Just "erlc-organ"
  ".purs" -> Just "purs-organ"
  ".ml"   -> Just "ocaml-organ"
  ".kk"   -> Just "koka-organ"
  ".swift" -> Just "swift-organ"
  ".agda" -> Just "agda-organ"
  ".fsx"  -> Just "fsharp-organ"
  ".scala" -> Just "scala3-organ"
  ".jl"   -> Just "julia-organ"
  ".zig"  -> Just "zig-organ"
  ".c"    -> Just "c-organ"
  ".cpp"  -> Just "cpp-organ"
  ".f90"  -> Just "fortran-organ"
  ".adb"  -> Just "ada-organ"
  ".sml"  -> Just "sml-organ"
  ".lisp" -> Just "cl-organ"
  ".scm"  -> Just "scheme-organ"
  ".pl"   -> Just "prolog-organ"
  ".lua"  -> Just "lua-organ-fe"
  ".fth"  -> Just "forth-organ-fe"
  _       -> Nothing

-- | Map manifest language name to the organ-bank executable name.
langToExe :: String -> Maybe String
langToExe lang = case lang of
  "haskell"     -> Just "ghc-organ"
  "rust"        -> Just "rustc-organ"
  "mercury"     -> Just "mmc-organ"
  "idris2"      -> Just "idris2-organ"
  "lean4"       -> Just "lean4-organ"
  "erlang"      -> Just "erlc-organ"
  "purescript"  -> Just "purs-organ"
  "ocaml"       -> Just "ocaml-organ"
  "koka"        -> Just "koka-organ"
  "swift"       -> Just "swift-organ"
  "agda"        -> Just "agda-organ"
  "fsharp"      -> Just "fsharp-organ"
  "scala3"      -> Just "scala3-organ"
  "julia"       -> Just "julia-organ"
  "zig"         -> Just "zig-organ"
  "c"           -> Just "c-organ"
  "cpp"         -> Just "cpp-organ"
  "fortran"     -> Just "fortran-organ"
  "ada"         -> Just "ada-organ"
  "sml"         -> Just "sml-organ"
  "common-lisp" -> Just "cl-organ"
  "scheme"      -> Just "scheme-organ"
  "prolog"      -> Just "prolog-organ"
  "lua"         -> Just "lua-organ-fe"
  "forth"       -> Just "forth-organ-fe"
  _             -> Nothing

-- | All supported extensions for the help message.
supportedExts :: [String]
supportedExts =
  [ ".hs", ".rs", ".m", ".idr", ".lean", ".erl", ".purs", ".ml"
  , ".kk", ".swift", ".agda", ".fsx", ".scala", ".jl", ".zig"
  , ".c", ".cpp", ".f90", ".adb", ".sml", ".lisp", ".scm"
  , ".pl", ".lua", ".fth"
  ]

usage :: String -> String
usage prog = unlines
  [ "Usage: " ++ prog ++ " [FLAGS] <source-file>"
  , "       " ++ prog ++ " [FLAGS] --project <dir>"
  , "       " ++ prog ++ " [FLAGS] --manifest <file>"
  , ""
  , "Single-file mode (default):"
  , "  Detect language from extension and run the appropriate shim."
  , ""
  , "Project mode:"
  , "  --project <dir>      Recursively discover source files in <dir>"
  , ""
  , "Manifest mode:"
  , "  --manifest <file>    Read a JSON manifest listing files and languages"
  , ""
  , "Output options (project/manifest mode):"
  , "  --bundle             Emit a single JSON array of all modules to stdout"
  , "  --outdir <dir>       Write one .organ.json file per source file"
  , "  (default)            Print each module's JSON separated by newlines"
  , ""
  , "Processing options:"
  , "  --resolve-imports    Cross-reference imports with exports across modules"
  , "  --validate           Pipe shim output through organ-validate"
  , "  --pretty             Pipe shim output through organ-validate --pretty"
  , ""
  , "Supported extensions:"
  , "  " ++ unwords supportedExts
  ]

-- * Options

data OutputMode = OutputLines | OutputBundle | OutputDir FilePath
  deriving (Show)

data Opts = Opts
  { optValidate       :: Bool
  , optPretty         :: Bool
  , optResolveImports :: Bool
  , optOutputMode     :: OutputMode
  , optProject        :: Maybe FilePath
  , optManifest       :: Maybe FilePath
  , optFile           :: Maybe FilePath
  }

defaultOpts :: Opts
defaultOpts = Opts False False False OutputLines Nothing Nothing Nothing

parseArgs :: [String] -> Either String Opts
parseArgs = go defaultOpts
  where
    go opts [] = Right opts
    go opts ("--validate" : rest) = go opts { optValidate = True } rest
    go opts ("--pretty" : rest)   = go opts { optPretty = True } rest
    go opts ("--resolve-imports" : rest) = go opts { optResolveImports = True } rest
    go opts ("--bundle" : rest)   = go opts { optOutputMode = OutputBundle } rest
    go opts ("--outdir" : d : rest) = go opts { optOutputMode = OutputDir d } rest
    go _    ("--outdir" : [])     = Left "--outdir requires a directory argument"
    go opts ("--project" : d : rest) = go opts { optProject = Just d } rest
    go _    ("--project" : [])    = Left "--project requires a directory argument"
    go opts ("--manifest" : f : rest) = go opts { optManifest = Just f } rest
    go _    ("--manifest" : [])   = Left "--manifest requires a file argument"
    go opts ("--help" : _)        = Right opts { optFile = Nothing, optProject = Nothing, optManifest = Nothing }
    go opts (arg : rest)
      | "--" `isPrefixOf` arg = go opts rest  -- skip unknown flags
      | otherwise = go opts { optFile = Just arg } rest

-- * Main

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr $ "Error: " ++ err
      exitFailure
    Right opts -> dispatch prog opts

dispatch :: String -> Opts -> IO ()
dispatch prog opts
  | Just dir <- optProject opts = runProjectMode opts dir
  | Just mf  <- optManifest opts = runManifestMode opts mf
  | Just src <- optFile opts = runSingleFile opts src
  | otherwise = do
      hPutStrLn stderr (usage prog)
      exitFailure

-- * Single-file mode (backward compatible)

runSingleFile :: Opts -> FilePath -> IO ()
runSingleFile opts srcFile = do
  let ext = takeExtension srcFile
  case extToExe ext of
    Nothing -> do
      hPutStrLn stderr $ "Error: unrecognized extension " ++ show ext
      hPutStrLn stderr $ "Supported: " ++ unwords supportedExts
      exitFailure
    Just exe -> do
      let wantPipe = optValidate opts || optPretty opts
          shimProc = (proc exe [srcFile])
            { std_out = if wantPipe then CreatePipe else Inherit }
      (_, mShimOut, _, shimPh) <- createProcess shimProc
      case mShimOut of
        Nothing -> do
          code <- waitForProcess shimPh
          exitWith code
        Just shimOut -> do
          let valArgs = if optPretty opts then ["--pretty"] else []
              valProc = (proc "organ-validate" valArgs)
                { std_in = UseHandle shimOut }
          (_, _, _, valPh) <- createProcess valProc
          shimCode <- waitForProcess shimPh
          valCode  <- waitForProcess valPh
          case (shimCode, valCode) of
            (ExitSuccess, ExitSuccess) -> pure ()
            (ExitFailure _, _)         -> exitWith shimCode
            (_, _)                     -> exitWith valCode

-- * Project mode

runProjectMode :: Opts -> FilePath -> IO ()
runProjectMode opts dir = do
  absDir <- makeAbsolute dir
  exists <- doesDirectoryExist absDir
  if not exists
    then do
      hPutStrLn stderr $ "Error: directory does not exist: " ++ absDir
      exitFailure
    else do
      files <- discoverSourceFiles absDir
      when (null files) $ do
        hPutStrLn stderr $ "Warning: no supported source files found in " ++ absDir
      results <- mapM (extractFile opts) files
      let tagged = zip files results
      when (optResolveImports opts) $
        resolveImports tagged
      emitResults opts absDir tagged

-- | Recursively discover source files with supported extensions.
discoverSourceFiles :: FilePath -> IO [FilePath]
discoverSourceFiles root = do
  entries <- listDirectory root
  paths <- concat <$> mapM (processEntry root) entries
  pure (sortBy (comparing takeExtension) paths)
  where
    processEntry base name = do
      let path = base </> name
      isDir <- doesDirectoryExist path
      if isDir
        then discoverSourceFiles path
        else pure [path | isSupportedExt (takeExtension name)]
    isSupportedExt ext = ext `elem` supportedExts

-- * Manifest mode

-- | A manifest entry: path + language.
data ManifestEntry = ManifestEntry
  { mePath   :: FilePath
  , meLang   :: String
  } deriving (Show)

runManifestMode :: Opts -> FilePath -> IO ()
runManifestMode opts manifestFile = do
  absManifest <- makeAbsolute manifestFile
  exists <- doesFileExist absManifest
  if not exists
    then do
      hPutStrLn stderr $ "Error: manifest file does not exist: " ++ absManifest
      exitFailure
    else do
      content <- readFile absManifest
      case parseManifest content of
        Left err -> do
          hPutStrLn stderr $ "Error parsing manifest: " ++ err
          exitFailure
        Right entries -> do
          let baseDir = takeDirectory absManifest
          results <- mapM (extractManifestEntry opts baseDir) entries
          let files = map (\e -> baseDir </> mePath e) entries
              tagged = zip files results
          when (optResolveImports opts) $
            resolveImports tagged
          emitResults opts baseDir tagged

-- | Minimal JSON manifest parser. Expects:
--   {"files": [{"path": "...", "language": "..."}, ...]}
parseManifest :: String -> Either String [ManifestEntry]
parseManifest input =
  case dropWhile isSpace input of
    '{' : rest -> do
      entries <- findFilesArray rest
      pure entries
    _ -> Left "Expected '{' at start of manifest"

-- | Find the "files" array and parse entries from it.
findFilesArray :: String -> Either String [ManifestEntry]
findFilesArray s = do
  rest <- expectStr "\"files\"" (dropWhile isSpace s)
  rest2 <- expectChar ':' (dropWhile isSpace rest)
  rest3 <- expectChar '[' (dropWhile isSpace rest2)
  parseEntries (dropWhile isSpace rest3)

parseEntries :: String -> Either String [ManifestEntry]
parseEntries s = case dropWhile isSpace s of
  ']' : _ -> Right []
  '{' : rest -> do
    (entry, rest2) <- parseOneEntry rest
    case dropWhile isSpace rest2 of
      ',' : rest3 -> do
        more <- parseEntries (dropWhile isSpace rest3)
        Right (entry : more)
      ']' : _ -> Right [entry]
      '}' : rest3 -> do
        -- closing brace of outer object, just return what we have
        let _ = rest3
        Right [entry]
      other -> Left $ "Expected ',' or ']' after manifest entry, got: " ++ take 30 other
  other -> Left $ "Expected '{' or ']' in manifest entries, got: " ++ take 30 other

parseOneEntry :: String -> Either String (ManifestEntry, String)
parseOneEntry s = do
  (fields, rest) <- parseFields (dropWhile isSpace s)
  entryPath <- case lookup "path" fields of
    Just p  -> Right p
    Nothing -> Left "Manifest entry missing \"path\" field"
  lang <- case lookup "language" fields of
    Just l  -> Right l
    Nothing -> Left "Manifest entry missing \"language\" field"
  Right (ManifestEntry entryPath lang, rest)

parseFields :: String -> Either String ([(String, String)], String)
parseFields s = case dropWhile isSpace s of
  '}' : rest -> Right ([], rest)
  '"' : rest -> do
    (key, rest2) <- parseQuotedString rest
    rest3 <- expectChar ':' (dropWhile isSpace rest2)
    (val, rest4) <- parseQuotedString (dropWhile isSpace (dropWhile isSpace rest3))
    case dropWhile isSpace rest4 of
      ',' : rest5 -> do
        (more, rest6) <- parseFields (dropWhile isSpace rest5)
        Right ((key, val) : more, rest6)
      '}' : rest5 -> Right ([(key, val)], rest5)
      other -> Left $ "Expected ',' or '}' in object, got: " ++ take 30 other
  other -> Left $ "Expected '\"' or '}' in object, got: " ++ take 30 other

parseQuotedString :: String -> Either String (String, String)
parseQuotedString s = case s of
  '"' : rest -> spanString "" rest
  other -> Left $ "Expected '\"', got: " ++ take 20 other

spanString :: String -> String -> Either String (String, String)
spanString acc ('"' : rest) = Right (reverse acc, rest)
spanString acc ('\\' : '"' : rest) = spanString ('"' : acc) rest
spanString acc ('\\' : '\\' : rest) = spanString ('\\' : acc) rest
spanString acc ('\\' : 'n' : rest) = spanString ('\n' : acc) rest
spanString acc (c : rest) = spanString (c : acc) rest
spanString _ [] = Left "Unterminated string in manifest"

expectStr :: String -> String -> Either String String
expectStr expected input
  | expected `isPrefixOf` input = Right (drop (length expected) input)
  | otherwise = Left $ "Expected " ++ expected ++ ", got: " ++ take 30 input

expectChar :: Char -> String -> Either String String
expectChar c (x : rest) | x == c = Right rest
expectChar c other = Left $ "Expected '" ++ [c] ++ "', got: " ++ take 20 other

-- | Extract a manifest entry by resolving its language to an exe.
extractManifestEntry :: Opts -> FilePath -> ManifestEntry -> IO (Either String String)
extractManifestEntry opts baseDir entry = do
  let filePath = baseDir </> mePath entry
  exists <- doesFileExist filePath
  if not exists
    then pure (Left $ "File not found: " ++ filePath)
    else case langToExe (meLang entry) of
      Nothing -> pure (Left $ "Unknown language: " ++ meLang entry)
      Just exe -> runShimCapture opts exe filePath

-- * Extraction

-- | Run a shim on a source file and capture the JSON output.
--   Returns Right json on success, Left error on failure.
extractFile :: Opts -> FilePath -> IO (Either String String)
extractFile opts srcFile = do
  let ext = takeExtension srcFile
  case extToExe ext of
    Nothing -> pure (Left $ "Unrecognized extension: " ++ ext)
    Just exe -> runShimCapture opts exe srcFile

-- | Run a shim executable, capture stdout. Optionally validate.
runShimCapture :: Opts -> String -> FilePath -> IO (Either String String)
runShimCapture opts exe srcFile = do
  let shimProc = (proc exe [srcFile])
        { std_out = CreatePipe
        , std_err = CreatePipe
        }
  (_, mOut, mErr, ph) <- createProcess shimProc
  output <- case mOut of
    Just h  -> hGetContents' h
    Nothing -> pure ""
  errOutput <- case mErr of
    Just h  -> hGetContents' h
    Nothing -> pure ""
  code <- waitForProcess ph
  case code of
    ExitSuccess -> do
      if optValidate opts || optPretty opts
        then validateOutput opts srcFile output
        else pure (Right output)
    ExitFailure n ->
      pure (Left $ exe ++ " failed (exit " ++ show n ++ ") on " ++ srcFile
                   ++ if null errOutput then "" else ": " ++ errOutput)

-- | Pipe output through organ-validate, return the result.
validateOutput :: Opts -> FilePath -> String -> IO (Either String String)
validateOutput opts srcFile input = do
  let valArgs = if optPretty opts then ["--pretty"] else []
      valProc = (proc "organ-validate" valArgs)
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  (mIn, mOut, mErr, ph) <- createProcess valProc
  case mIn of
    Just h -> do
      hPutStrLn h input
      hClose h
    Nothing -> pure ()
  output <- case mOut of
    Just h  -> hGetContents' h
    Nothing -> pure ""
  errOutput <- case mErr of
    Just h  -> hGetContents' h
    Nothing -> pure ""
  code <- waitForProcess ph
  case code of
    ExitSuccess -> pure (Right output)
    ExitFailure n ->
      pure (Left $ "organ-validate failed (exit " ++ show n ++ ") on " ++ srcFile
                   ++ if null errOutput then "" else ": " ++ errOutput)

-- * Import resolution

-- | Cross-reference imports with exports across all successfully extracted modules.
--   Prints warnings for unresolved imports to stderr.
resolveImports :: [(FilePath, Either String String)] -> IO ()
resolveImports tagged = do
  -- Collect all exports from successful extractions
  let successes = [(f, json) | (f, Right json) <- tagged]
  -- Parse module names and exports from JSON (lightweight: just look for key fields)
  allExports <- concat <$> mapM collectExports successes
  let exportSet = nub allExports
  -- Check imports from each module
  mapM_ (checkImports exportSet) successes
  where
    collectExports :: (FilePath, String) -> IO [String]
    collectExports (_f, json) = do
      let exports = extractJsonStringArray "exports" json
      pure exports

    checkImports :: [String] -> (FilePath, String) -> IO ()
    checkImports exports (f, json) = do
      let imports = extractJsonQNameTexts "imports" json
      let unresolved = [imp | imp <- imports, imp `notElem` exports]
      mapM_ (\imp -> hPutStrLn stderr $
        "Warning: unresolved import '" ++ imp ++ "' in " ++ f) unresolved

-- | Extract string values from a JSON array field (lightweight text scan).
--   Looks for "exports": ["name1", "name2", ...]
extractJsonStringArray :: String -> String -> [String]
extractJsonStringArray key json =
  case findKeyInJson key json of
    Just rest -> parseSimpleStringArray rest
    Nothing   -> []

-- | Extract the "text" fields from a JSON array of QName objects under the given key.
--   Looks for "imports": [{"module": "...", "name": {"text": "foo", ...}}, ...]
extractJsonQNameTexts :: String -> String -> [String]
extractJsonQNameTexts key json =
  case findKeyInJson key json of
    Just rest -> extractTextFields rest
    Nothing   -> []

-- | Find a key in JSON and return the rest starting at the value.
findKeyInJson :: String -> String -> Maybe String
findKeyInJson key json =
  let needle = "\"" ++ key ++ "\""
  in case breakOnSubstring needle json of
    Just rest ->
      let afterKey = dropWhile isSpace (drop (length needle) rest)
      in case afterKey of
        ':' : val -> Just (dropWhile isSpace val)
        _         -> Nothing
    Nothing -> Nothing

breakOnSubstring :: String -> String -> Maybe String
breakOnSubstring _needle [] = Nothing
breakOnSubstring needle hay@(_ : rest)
  | needle `isPrefixOf` hay = Just hay
  | otherwise = breakOnSubstring needle rest

-- | Parse a simple JSON string array like ["a", "b", "c"]
parseSimpleStringArray :: String -> [String]
parseSimpleStringArray s = case dropWhile isSpace s of
  '[' : rest -> go (dropWhile isSpace rest)
  _ -> []
  where
    go (']' : _) = []
    go ('"' : rest) = case break (== '"') rest of
      (str, '"' : rest2) -> str : go (dropWhile (\c -> isSpace c || c == ',') rest2)
      _ -> []
    go _ = []

-- | Extract all "text" field values that appear within the given JSON fragment
--   (up to the closing ']' of the imports array).
extractTextFields :: String -> [String]
extractTextFields s = case dropWhile isSpace s of
  '[' : rest -> goText rest
  _ -> []
  where
    goText [] = []
    goText (']' : _) = []
    goText str = case breakOnSubstring "\"text\"" str of
      Just found ->
        let afterKey = drop 6 found  -- skip "text"
            afterColon = dropWhile isSpace (dropWhile (/= ':') afterKey)
            rest2 = drop 1 afterColon  -- skip ':'
            trimmed = dropWhile isSpace rest2
        in case trimmed of
          '"' : rest3 -> case break (== '"') rest3 of
            (val, '"' : rest4) -> val : goText rest4
            _ -> []
          _ -> goText (drop 1 str)
      Nothing -> []

-- * Output

emitResults :: Opts -> FilePath -> [(FilePath, Either String String)] -> IO ()
emitResults opts baseDir tagged = do
  failRef <- newIORef False
  case optOutputMode opts of
    OutputBundle -> do
      putStr "["
      let successes = [(f, json) | (f, Right json) <- tagged]
          failures  = [(f, err)  | (f, Left err)  <- tagged]
      mapM_ (\(f, err) -> do
        hPutStrLn stderr $ "Error [" ++ f ++ "]: " ++ err
        writeIORef failRef True) failures
      putStr (commaJoin (map snd successes))
      putStrLn "]"

    OutputDir outdir -> do
      createDirectoryIfMissing True outdir
      mapM_ (\(srcFile, result) -> do
        let relPath = makeRelative baseDir srcFile
            outFile = outdir </> (relPath -<.> "organ.json")
        case result of
          Right json -> do
            createDirectoryIfMissing True (takeDirectory outFile)
            writeFile outFile json
            hPutStrLn stderr $ "Wrote: " ++ outFile
          Left err -> do
            hPutStrLn stderr $ "Error [" ++ srcFile ++ "]: " ++ err
            writeIORef failRef True) tagged

    OutputLines -> do
      mapM_ (\(srcFile, result) ->
        case result of
          Right json -> putStrLn json
          Left err -> do
            hPutStrLn stderr $ "Error [" ++ srcFile ++ "]: " ++ err
            writeIORef failRef True) tagged

  failed <- readIORef failRef
  when failed exitFailure

-- | Join strings with commas (no trailing comma).
commaJoin :: [String] -> String
commaJoin [] = ""
commaJoin [x] = x
commaJoin (x : xs) = x ++ "," ++ commaJoin xs

-- | 'when' from Control.Monad (avoiding import for minimal deps).
when :: Bool -> IO () -> IO ()
when True  act = act
when False _   = pure ()
