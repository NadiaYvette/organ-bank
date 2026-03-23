-- | Extract type basis from SML via MLton and parse source for definitions.
-- MLton has no IR dump flag, so this shim combines:
-- 1. Type signatures from `mlton -stop tc -show-basis`
-- 2. Source-level function structure parsing
module OrganBank.SmlShim (extractOrganIR) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (takeBaseName)
import Control.Exception (catch, SomeException)

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
  -- MLton needs an .mlb file for compilation
  let mlbPath = "/tmp/organ-bank-sml.mlb"
      mlbContent = "$(SML_LIB)/basis/basis.mlb\n" ++ inputPath ++ "\n"
  writeFile mlbPath mlbContent
  -- Get type basis
  (ec, basisOut, basisErr) <- readProcessWithExitCode "mlton"
    ["-stop", "tc", "-show-basis", "/dev/stdout", mlbPath] ""
  removeFile mlbPath `catch` (\(_ :: SomeException) -> return ())
  -- Parse source file regardless of whether basis worked
  source <- TIO.readFile inputPath
  let basis = case ec of
        ExitSuccess -> T.pack basisOut
        _ -> ""
      types = parseBasis basis
      defs = parseSmlSource source types
      modName = takeBaseName inputPath
      json = emitOrganIR modName inputPath defs
  return $ Right json

data SmlDef = SmlDef
  { sdName   :: Text
  , sdType   :: Text  -- from basis, or "any"
  , sdBody   :: Text  -- source text of the body
  , sdArity  :: Int
  } deriving (Show)

-- Parse MLton -show-basis output: "val factorial: int -> int"
parseBasis :: Text -> [(Text, Text)]
parseBasis t = concatMap parseLine (T.lines t)
  where
    parseLine l
      | "val " `T.isPrefixOf` T.strip l =
        let rest = T.drop 4 (T.strip l)
            (name, afterName) = T.breakOn ":" rest
            ty = T.strip (T.drop 1 afterName)
        in [(T.strip name, ty)]
      | otherwise = []

-- Parse SML source for fun/val declarations
parseSmlSource :: Text -> [(Text, Text)] -> [SmlDef]
parseSmlSource source types = parseFunDecls (T.lines source) types

parseFunDecls :: [Text] -> [(Text, Text)] -> [SmlDef]
parseFunDecls [] _ = []
parseFunDecls (l:ls) types
  | "fun " `T.isPrefixOf` T.strip l =
    let (name, arity, bodyLines, rest) = parseFunBinding l ls
        ty = lookupType name types
        body = T.unlines (l : bodyLines)
    in SmlDef name ty body arity : parseFunDecls rest types
  | "val " `T.isPrefixOf` T.strip l && "fn " `T.isInfixOf` l =
    let rest2 = T.drop 4 (T.strip l)
        (name, _) = T.breakOn " " rest2
        ty = lookupType name types
    in SmlDef (T.strip name) ty l 1 : parseFunDecls ls types
  | otherwise = parseFunDecls ls types

parseFunBinding :: Text -> [Text] -> (Text, Int, [Text], [Text])
parseFunBinding firstLine rest =
  let stripped = T.strip firstLine
      afterFun = T.drop 4 stripped  -- drop "fun "
      -- Handle optional rec
      afterRec = if "rec " `T.isPrefixOf` afterFun then T.drop 4 afterFun else afterFun
      name = T.takeWhile (\c -> c /= ' ' && c /= '(') afterRec
      -- Count parameters (words between name and =)
      afterName = T.drop (T.length name) afterRec
      beforeEq = T.takeWhile (/= '=') afterName
      params = filter (not . T.null) $ T.words (T.strip beforeEq)
      arity = length params
      -- Collect continuation lines (| clauses)
      (contLines, remaining) = collectContinuation rest
  in (T.strip name, arity, contLines, remaining)

collectContinuation :: [Text] -> ([Text], [Text])
collectContinuation [] = ([], [])
collectContinuation (l:ls)
  | "  |" `T.isPrefixOf` l || "  | " `T.isPrefixOf` l =
    let (more, rest) = collectContinuation ls in (l:more, rest)
  | "and " `T.isPrefixOf` T.strip l =
    -- another clause of the same fun declaration
    let (more, rest) = collectContinuation ls in (l:more, rest)
  | otherwise = ([], l:ls)

lookupType :: Text -> [(Text, Text)] -> Text
lookupType name types = case lookup name types of
  Just t  -> t
  Nothing -> "any"

emitOrganIR :: String -> FilePath -> [SmlDef] -> Text
emitOrganIR modName srcFile defs = T.concat
  [ "{\n  \"source_language\": \"sml\",\n"
  , "  \"module_name\": ", jsonStr (T.pack modName), ",\n"
  , "  \"source_file\": ", jsonStr (T.pack srcFile), ",\n"
  , "  \"compiler_version\": \"mlton-basis\",\n"
  , "  \"definitions\": [\n"
  , T.intercalate ",\n" (map emitDef defs)
  , "\n  ]\n}\n"
  ]

emitDef :: SmlDef -> Text
emitDef d = T.concat
  [ "    {\n"
  , "      \"name\": {\"module\": \"\", \"text\": ", jsonStr (sdName d), ", \"unique\": 0},\n"
  , "      \"type\": ", emitType (sdType d), ",\n"
  , "      \"expr\": {\"tag\": \"source\", \"text\": ", jsonStr (sdBody d), "},\n"
  , "      \"sort\": \"fun\",\n      \"visibility\": \"public\",\n"
  , "      \"arity\": ", T.pack (show (sdArity d)), "\n    }"
  ]

emitType :: Text -> Text
emitType "any" = "{\"tag\": \"any\"}"
emitType t
  | " -> " `T.isInfixOf` t =
    let parts = T.splitOn " -> " t
        params = init parts
        result = last parts
    in T.concat
      [ "{\"tag\": \"fn\", \"params\": ["
      , T.intercalate ", " (map (\p -> T.concat ["{\"tag\": \"con\", \"name\": ", jsonStr p, "}"]) params)
      , "], \"result\": {\"tag\": \"con\", \"name\": ", jsonStr result, "}}"
      ]
  | otherwise = T.concat ["{\"tag\": \"con\", \"name\": ", jsonStr t, "}"]

jsonStr :: Text -> Text
jsonStr t = T.concat ["\"", T.concatMap esc t, "\""]
  where esc '"' = "\\\""; esc '\\' = "\\\\"; esc '\n' = "\\n"; esc '\t' = "\\t"; esc c = T.singleton c
