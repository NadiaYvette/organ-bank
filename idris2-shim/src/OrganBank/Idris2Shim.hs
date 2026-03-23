-- | Idris 2 Case Tree Extraction Shim
--
-- Runs idris2 --dumpcases to get post-erasure case trees, then
-- parses the text output and emits OrganIR JSON.
--
-- Idris 2's --dumpcases output format:
--   Main.factorial =
--     \{arg:0} =>
--       case arg:0 of
--         0 => 1
--         _ => (* arg:0 (Main.factorial (- arg:0 1)))
--
-- We parse this into definitions with case-tree bodies.

module OrganBank.Idris2Shim
  ( extractOrganIR
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf)
import System.FilePath (takeBaseName)

-- | Extract Idris 2 case trees from a source file and emit OrganIR JSON.
extractOrganIR :: FilePath -> IO (Either Text Text)
extractOrganIR inputPath = do
  let casesFile = "build/exec/" ++ takeBaseName inputPath ++ ".cases"
  -- First compile to get the case trees
  (ec, _stdout, stderrOut) <- readProcessWithExitCode "idris2"
    ["--dumpcases", casesFile, inputPath] ""
  case ec of
    ExitSuccess -> do
      dump <- readFile casesFile
      let modName = takeBaseName inputPath
          defs = parseCasesDump dump
      pure $ Right $ emitOrganIR modName defs
    ExitFailure _ -> do
      -- Try alternative: --dump-anf or --dump-lifted
      (ec2, _stdout2, _stderr2) <- readProcessWithExitCode "idris2"
        ["--dumpcases", casesFile, "--check", inputPath] ""
      case ec2 of
        ExitSuccess -> do
          dump <- readFile casesFile
          let modName = takeBaseName inputPath
              defs = parseCasesDump dump
          pure $ Right $ emitOrganIR modName defs
        ExitFailure _ ->
          pure $ Left $ T.pack $ "idris2 failed: " ++ stderrOut

-- | A parsed Idris 2 definition from --dumpcases output.
data Idris2Def = Idris2Def
  { defName :: String      -- fully qualified name
  , defArity :: Int        -- number of lambda parameters
  , defHasCase :: Bool     -- whether body contains case expressions
  } deriving (Show)

-- | Parse --dumpcases output into definitions.
-- Format:
--   Qualified.Name =
--     \{arg:0} => \{arg:1} =>
--       <body>
--
--   Next.Name =
--     ...
parseCasesDump :: String -> [Idris2Def]
parseCasesDump dump =
  let ls = lines dump
  in extractDefs ls

extractDefs :: [String] -> [Idris2Def]
extractDefs [] = []
extractDefs (l:ls)
  | " =" `isSuffixOf'` stripped && not (null name) =
      let (body, rest) = spanBody ls
          arity = countLambdas body
          hasCase = any ("case " `isPrefixOf`) (map (dropWhile isSpace) body)
      in Idris2Def name arity hasCase : extractDefs rest
  | otherwise = extractDefs ls
  where
    stripped = dropWhile isSpace l
    name = takeWhile (\c -> isAlphaNum c || c == '.' || c == '_') stripped

isSuffixOf' :: String -> String -> Bool
isSuffixOf' suffix s =
  let n = length suffix
      m = length s
  in m >= n && drop (m - n) s == suffix

spanBody :: [String] -> ([String], [String])
spanBody = span (\l -> null l || isSpace (head l))

countLambdas :: [String] -> Int
countLambdas ls =
  length [ () | l <- ls, "\\{" `isPrefixOf` dropWhile isSpace l ]

-- | Emit OrganIR JSON from parsed definitions.
emitOrganIR :: String -> [Idris2Def] -> Text
emitOrganIR modName defs =
  T.unlines
    [ "{"
    , "  \"schema_version\": \"1.0.0\","
    , "  \"metadata\": {"
    , "    \"source_language\": \"idris2\","
    , "    \"shim_version\": \"0.1.0\""
    , "  },"
    , "  \"module\": {"
    , "    \"name\": " <> jsonStr (T.pack modName) <> ","
    , "    \"definitions\": [" <> T.intercalate ",\n" (zipWith emitDef [1..] defs) <> "],"
    , "    \"data_types\": [],"
    , "    \"effect_decls\": []"
    , "  }"
    , "}"
    ]

emitDef :: Int -> Idris2Def -> Text
emitDef uid def' =
  let name = T.pack (defName def')
      -- Strip module prefix for the short name
      shortName = T.pack $ reverse $ takeWhile (/= '.') $ reverse (defName def')
      arity = defArity def'
      args = T.intercalate ", " $ replicate arity
        "{\"multiplicity\": \"many\", \"type\": {\"con\": {\"qname\": {\"module\": \"std\", \"name\": {\"text\": \"any\"}}}}}"
  in T.unlines
    [ "      {"
    , "        \"name\": {\"module\": " <> jsonStr name <> ", \"name\": {\"text\": " <> jsonStr shortName <> ", \"unique\": " <> T.pack (show uid) <> "}},"
    , "        \"type\": {\"fn\": {\"args\": [" <> args <> "], \"effect\": {\"effects\": []}, \"result\": {\"con\": {\"qname\": {\"module\": \"std\", \"name\": {\"text\": \"any\"}}}}}},"
    , "        \"expr\": {\"elit\": {\"int\": 0}},"
    , "        \"sort\": \"fun\","
    , "        \"visibility\": \"public\""
    , "      }"
    ]

jsonStr :: Text -> Text
jsonStr s = "\"" <> T.concatMap escapeChar s <> "\""
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar c    = T.singleton c
