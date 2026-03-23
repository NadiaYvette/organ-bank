-- | Mercury HLDS Extraction Shim
--
-- Runs mmc --dump-hlds 50 to get the HLDS dump after mode/determinism
-- analysis, then parses the text output and emits OrganIR JSON.
--
-- Based on Frankenstein's MercuryBridge/HldsParse.hs.

module OrganBank.MmcShim
  ( extractOrganIR
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import System.FilePath (takeBaseName)
import System.IO (hPutStrLn, stderr)
import qualified Control.Exception

-- | Extract Mercury HLDS from a source file and emit OrganIR JSON.
extractOrganIR :: FilePath -> IO (Either Text Text)
extractOrganIR inputPath = do
  -- Mercury compiler dumps HLDS to stderr with --dump-hlds
  (ec, _stdout, stderrOut) <- readProcessWithExitCode "mmc"
    ["--dump-hlds", "50", inputPath] ""
  -- mmc may return non-zero even on success with --dump-hlds
  let hldsText = stderrOut
  -- Also check for .hlds file
  let hldsFile = takeBaseName inputPath ++ ".hlds_dump"
  hldsFromFile <- tryReadFile hldsFile
  let dump = case hldsFromFile of
        Just t  -> t
        Nothing -> hldsText
  case ec of
    ExitSuccess -> pure ()
    ExitFailure _ ->
      hPutStrLn stderr "mmc exited with error (dump may still be usable)"
  if null dump
    then pure $ Left "No HLDS output from mmc"
    else do
      let modName = takeBaseName inputPath
          preds = parseHldsDump dump
      pure $ Right $ emitOrganIR modName preds

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile fp = do
  result <- Control.Exception.try @IOError (readFile fp >>= \c -> length c `seq` pure c)
  pure $ either (const Nothing) Just result

-- | A parsed Mercury predicate from HLDS dump.
data HldsPred = HldsPred
  { predName :: String
  , predArity :: Int
  , predDet :: String    -- "det", "semidet", "multi", "nondet", "erroneous", "failure"
  , predModes :: [String]  -- mode declarations
  } deriving (Show)

-- | Parse the HLDS dump text to extract predicate declarations.
parseHldsDump :: String -> [HldsPred]
parseHldsDump dump =
  let ls = lines dump
  in extractPreds ls

extractPreds :: [String] -> [HldsPred]
extractPreds [] = []
extractPreds (l:ls)
  | ":- pred " `isPrefixOf` stripped =
      let predDecl = drop 8 stripped  -- drop ":- pred "
          (name, rest) = break (== '(') predDecl
          arity = countArgs rest
          (det, modes, remaining) = findDetAndModes ls
      in HldsPred (trim name) arity det modes : extractPreds remaining
  | otherwise = extractPreds ls
  where
    stripped = dropWhile isSpace l

countArgs :: String -> Int
countArgs s =
  let inner = takeWhile (/= ')') (drop 1 s)  -- drop the '('
  in if null inner then 0
     else length (filter (== ',') inner) + 1

findDetAndModes :: [String] -> (String, [String], [String])
findDetAndModes ls =
  let (block, rest) = span (\l' -> not (":- pred " `isPrefixOf` dropWhile isSpace l') && not (null l')) ls
      det = findDet block
      modes = findModes block
  in (det, modes, rest)

findDet :: [String] -> String
findDet [] = "det"
findDet (l:ls)
  | "is det"      `isInfixOf'` l = "det"
  | "is semidet"  `isInfixOf'` l = "semidet"
  | "is multi"    `isInfixOf'` l = "multi"
  | "is nondet"   `isInfixOf'` l = "nondet"
  | "is erroneous" `isInfixOf'` l = "erroneous"
  | "is failure"  `isInfixOf'` l = "failure"
  | otherwise = findDet ls

isInfixOf' :: String -> String -> Bool
isInfixOf' needle haystack = any (isPrefixOf needle) (tails' haystack)
  where tails' [] = [[]]
        tails' s@(_:xs) = s : tails' xs

findModes :: [String] -> [String]
findModes ls =
  [ trim l | l <- ls, ":- mode " `isPrefixOf` dropWhile isSpace l ]

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Convert Mercury determinism to OrganIR effect.
detToEffect :: String -> Text
detToEffect "det"       = "[]"
detToEffect "semidet"   = "[{\"module\": \"std\", \"name\": {\"text\": \"exn\"}}]"
detToEffect "multi"     = "[{\"module\": \"std\", \"name\": {\"text\": \"choice\"}}]"
detToEffect "nondet"    = "[{\"module\": \"std\", \"name\": {\"text\": \"exn\"}}, {\"module\": \"std\", \"name\": {\"text\": \"choice\"}}]"
detToEffect "erroneous" = "[{\"module\": \"std\", \"name\": {\"text\": \"exn\"}}]"
detToEffect "failure"   = "[{\"module\": \"std\", \"name\": {\"text\": \"exn\"}}]"
detToEffect _           = "[]"

-- | Emit OrganIR JSON from parsed predicates.
emitOrganIR :: String -> [HldsPred] -> Text
emitOrganIR modName preds =
  T.unlines
    [ "{"
    , "  \"schema_version\": \"1.0.0\","
    , "  \"metadata\": {"
    , "    \"source_language\": \"mercury\","
    , "    \"shim_version\": \"0.1.0\""
    , "  },"
    , "  \"module\": {"
    , "    \"name\": " <> jsonStr (T.pack modName) <> ","
    , "    \"definitions\": [" <> T.intercalate ",\n" (zipWith emitPred [1..] preds) <> "],"
    , "    \"data_types\": [],"
    , "    \"effect_decls\": []"
    , "  }"
    , "}"
    ]

emitPred :: Int -> HldsPred -> Text
emitPred uid pred' =
  let name = T.pack (predName pred')
      eff  = detToEffect (predDet pred')
      arity = predArity pred'
      args = T.intercalate ", " $ replicate arity
        "{\"multiplicity\": \"many\", \"type\": {\"con\": {\"qname\": {\"module\": \"std\", \"name\": {\"text\": \"any\"}}}}}"
  in T.unlines
    [ "      {"
    , "        \"name\": {\"module\": \"\", \"name\": {\"text\": " <> jsonStr name <> ", \"unique\": " <> T.pack (show uid) <> "}},"
    , "        \"type\": {\"fn\": {\"args\": [" <> args <> "], \"effect\": {\"effects\": " <> eff <> "}, \"result\": {\"con\": {\"qname\": {\"module\": \"std\", \"name\": {\"text\": \"any\"}}}}}},"
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
