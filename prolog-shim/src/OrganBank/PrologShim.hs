-- | Extract GNU Prolog WAM bytecode and emit OrganIR JSON.
module OrganBank.PrologShim (extractOrganIR) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.Char (isSpace)

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
  (ec, out, err) <- readProcessWithExitCode "gplc"
    ["-W", inputPath] ""
  case ec of
    ExitFailure _ -> return $ Left $ "gplc failed: " ++ err
    ExitSuccess -> do
      let defs = parseWam (T.pack out)
          modName = takeBaseName inputPath
          json = emitOrganIR modName inputPath defs
      return $ Right json

takeBaseName :: FilePath -> String
takeBaseName p =
  let base = reverse $ takeWhile (/= '/') $ reverse p
  in case break (== '.') base of
       (name, _) -> name

data WamPred = WamPred
  { wpName       :: Text
  , wpArity      :: Int
  , wpVisibility :: Text
  , wpInstructions :: [Text]
  } deriving (Show)

-- Parse gplc -W output:
-- predicate(name/arity,nclauses,static,public/private,monofile,global,[
--     instruction1,
--     instruction2,
--     ...])
parseWam :: Text -> [WamPred]
parseWam input = parsePreds (T.lines input)

parsePreds :: [Text] -> [WamPred]
parsePreds [] = []
parsePreds (l:ls)
  | "predicate(" `T.isPrefixOf` T.strip l =
    let (instrLines, rest) = collectUntilClose ls
        allText = T.concat (l : instrLines)
        pred = parsePredicate allText
    in pred : parsePreds rest
  | otherwise = parsePreds ls

collectUntilClose :: [Text] -> ([Text], [Text])
collectUntilClose [] = ([], [])
collectUntilClose (l:ls)
  | "])" `T.isInfixOf` l = ([l], ls)
  | otherwise = let (more, rest) = collectUntilClose ls in (l:more, rest)

parsePredicate :: Text -> WamPred
parsePredicate t =
  let -- Extract predicate(name/arity, ...)
      afterPred = T.drop 10 (T.strip t)  -- drop "predicate("
      -- Get name/arity
      (nameArity, rest1) = T.breakOn "," afterPred
      (name, arityStr) = T.breakOn "/" nameArity
      arity = case reads (T.unpack (T.drop 1 arityStr)) :: [(Int, String)] of
                [(n, _)] -> n
                _        -> 0
      -- Find visibility (public or private)
      vis = if "public" `T.isInfixOf` rest1 then "public" else "private"
      -- Extract instruction list between [ and ]
      instrText = extractBetween '[' ']' rest1
      instrs = parseInstructions instrText
  in WamPred (T.strip name) arity vis instrs

extractBetween :: Char -> Char -> Text -> Text
extractBetween open close t =
  let afterOpen = T.drop 1 $ T.dropWhile (/= open) t
      beforeClose = T.takeWhile (/= close) afterOpen
  in beforeClose

parseInstructions :: Text -> [Text]
parseInstructions t =
  let parts = splitInstructions (T.strip t)
  in filter (not . T.null) $ map T.strip parts

-- Split on commas but respect parentheses
splitInstructions :: Text -> [Text]
splitInstructions t = go t 0 "" []
  where
    go remaining depth current acc
      | T.null remaining = reverse (T.pack (reverse current) : acc)
      | otherwise =
        let c = T.head remaining
            rest = T.tail remaining
        in case c of
             '(' -> go rest (depth + 1) (c : current) acc
             ')' -> go rest (depth - 1) (c : current) acc
             ',' | depth == 0 -> go rest 0 "" (T.pack (reverse current) : acc)
             '\n' -> go rest depth current acc
             _ -> go rest depth (c : current) acc

emitOrganIR :: String -> FilePath -> [WamPred] -> Text
emitOrganIR modName srcFile defs = T.concat
  [ "{\n  \"source_language\": \"prolog\",\n"
  , "  \"module_name\": ", jsonStr (T.pack modName), ",\n"
  , "  \"source_file\": ", jsonStr (T.pack srcFile), ",\n"
  , "  \"compiler_version\": \"gprolog-wam\",\n"
  , "  \"definitions\": [\n"
  , T.intercalate ",\n" (map emitDef defs)
  , "\n  ]\n}\n"
  ]

emitDef :: WamPred -> Text
emitDef p = T.concat
  [ "    {\n"
  , "      \"name\": {\"module\": \"\", \"text\": "
  , jsonStr (T.concat [wpName p, "/", T.pack (show (wpArity p))])
  , ", \"unique\": 0},\n"
  , "      \"type\": {\"tag\": \"any\"},\n"
  , "      \"expr\": {\"tag\": \"wam_body\", \"instructions\": [\n"
  , T.intercalate ",\n" (map (\i -> T.concat ["          ", jsonStr i]) (wpInstructions p))
  , "\n      ]},\n"
  , "      \"sort\": \"fun\",\n"
  , "      \"visibility\": ", jsonStr (wpVisibility p), ",\n"
  , "      \"arity\": ", T.pack (show (wpArity p)), "\n    }"
  ]

jsonStr :: Text -> Text
jsonStr t = T.concat ["\"", T.concatMap esc t, "\""]
  where esc '"' = "\\\""; esc '\\' = "\\\\"; esc '\n' = "\\n"; esc '\t' = "\\t"; esc c = T.singleton c
