-- | Extract LLVM IR from C source via clang and emit OrganIR JSON.
module OrganBank.CShim (extractOrganIR) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
  (ec, out, err) <- readProcessWithExitCode "clang"
    ["-emit-llvm", "-S", "-O2", "-o", "-", inputPath] ""
  case ec of
    ExitFailure _ -> return $ Left $ "clang failed: " ++ err
    ExitSuccess -> do
      let defs = parseLlvmIR (T.pack out)
          modName = takeBaseName inputPath
          json = emitOrganIR modName inputPath defs
      return $ Right json

takeBaseName :: FilePath -> String
takeBaseName = reverse . takeWhile (/= '/') . reverse

-- | A parsed LLVM function definition.
data LlvmFunc = LlvmFunc
  { lfName    :: Text
  , lfRetTy   :: Text
  , lfParams  :: [(Text, Text)]  -- (type, name)
  , lfBlocks  :: [(Text, [Text])]  -- (label, instructions)
  } deriving (Show)

parseLlvmIR :: Text -> [LlvmFunc]
parseLlvmIR ir = parseDefines (T.lines ir)

parseDefines :: [Text] -> [LlvmFunc]
parseDefines [] = []
parseDefines (l:ls)
  | "define " `T.isPrefixOf` l =
    let (bodyLines, rest) = collectBody ls 1
        func = parseDefine l bodyLines
    in func : parseDefines rest
  | otherwise = parseDefines ls

collectBody :: [Text] -> Int -> ([Text], [Text])
collectBody [] _ = ([], [])
collectBody (l:ls) depth
  | "}" `T.isSuffixOf` T.strip l && depth <= 1 = ([], ls)
  | "{" `T.isSuffixOf` T.strip l = let (b, r) = collectBody ls (depth + 1) in (l:b, r)
  | "}" `T.isSuffixOf` T.strip l = let (b, r) = collectBody ls (depth - 1) in (l:b, r)
  | otherwise = let (b, r) = collectBody ls depth in (l:b, r)

parseDefine :: Text -> [Text] -> LlvmFunc
parseDefine header bodyLines =
  let stripped = T.strip header
      -- "define <retty> @name(<params>) ... {"
      afterDefine = T.drop 7 stripped  -- drop "define "
      -- Handle linkage keywords
      afterLinkage = skipLlvmKeywords afterDefine
      (retTy, rest1) = T.breakOn " @" afterLinkage
      nameAndParams = T.drop 2 rest1  -- drop " @"
      (name, rest2) = T.breakOn "(" nameAndParams
      paramStr = T.takeWhile (/= ')') (T.drop 1 rest2)
      params = parseParams paramStr
      blocks = parseBlocks bodyLines
  in LlvmFunc (T.strip name) (T.strip retTy) params blocks

skipLlvmKeywords :: Text -> Text
skipLlvmKeywords t =
  let kws = ["dso_local ", "internal ", "private ", "linkonce_odr ", "weak ", "hidden ",
             "noundef ", "signext ", "zeroext "]
      strip1 s = case filter (`T.isPrefixOf` s) kws of
                   (k:_) -> strip1 (T.drop (T.length k) s)
                   []    -> s
  in strip1 t

parseParams :: Text -> [(Text, Text)]
parseParams t
  | T.null (T.strip t) = []
  | otherwise = concatMap parseOneParam (T.splitOn "," t)
  where
    parseOneParam p =
      let ws = T.words (T.strip p)
      in case ws of
           []  -> []
           [_] -> [(T.strip p, "")]
           _   -> [(T.unwords (init ws), last ws)]

parseBlocks :: [Text] -> [(Text, [Text])]
parseBlocks = go "entry" []
  where
    go lbl acc [] = [(lbl, reverse acc)]
    go lbl acc (l:ls)
      | ":" `T.isSuffixOf` T.strip l && not (T.isPrefixOf "  " l) =
        let newLbl = T.strip (T.dropEnd 1 (T.strip l))
        in (lbl, reverse acc) : go newLbl [] ls
      | otherwise =
        let trimmed = T.strip l
        in if T.null trimmed || ";" `T.isPrefixOf` trimmed
           then go lbl acc ls
           else go lbl (trimmed : acc) ls

-- JSON emission
emitOrganIR :: String -> FilePath -> [LlvmFunc] -> Text
emitOrganIR modName srcFile defs = T.concat
  [ "{\n"
  , "  \"source_language\": \"c\",\n"
  , "  \"module_name\": ", jsonStr (T.pack modName), ",\n"
  , "  \"source_file\": ", jsonStr (T.pack srcFile), ",\n"
  , "  \"compiler_version\": \"clang-llvm\",\n"
  , "  \"definitions\": [\n"
  , T.intercalate ",\n" (map emitDef defs)
  , "\n  ]\n"
  , "}\n"
  ]

emitDef :: LlvmFunc -> Text
emitDef f = T.concat
  [ "    {\n"
  , "      \"name\": {\"module\": \"\", \"text\": ", jsonStr (lfName f), ", \"unique\": 0},\n"
  , "      \"type\": ", emitFnType f, ",\n"
  , "      \"expr\": ", emitBody (lfBlocks f), ",\n"
  , "      \"sort\": \"fun\",\n"
  , "      \"visibility\": \"public\",\n"
  , "      \"arity\": ", T.pack (show (length (lfParams f))), "\n"
  , "    }"
  ]

emitFnType :: LlvmFunc -> Text
emitFnType f = T.concat
  [ "{\"tag\": \"fn\", \"params\": ["
  , T.intercalate ", " [T.concat ["{\"tag\": \"con\", \"name\": ", jsonStr ty, "}"] | (ty, _) <- lfParams f]
  , "], \"result\": {\"tag\": \"con\", \"name\": ", jsonStr (lfRetTy f), "}}"
  ]

emitBody :: [(Text, [Text])] -> Text
emitBody blocks = T.concat
  [ "{\"tag\": \"ssa_body\", \"blocks\": [\n"
  , T.intercalate ",\n" (map emitBlock blocks)
  , "\n      ]}"
  ]

emitBlock :: (Text, [Text]) -> Text
emitBlock (lbl, stmts) = T.concat
  [ "        {\"label\": ", jsonStr lbl, ", \"stmts\": [\n"
  , T.intercalate ",\n" (map (\s -> T.concat ["          ", jsonStr s]) stmts)
  , "\n        ]}"
  ]

jsonStr :: Text -> Text
jsonStr t = T.concat ["\"", T.concatMap escChar t, "\""]
  where
    escChar '"'  = "\\\""
    escChar '\\' = "\\\\"
    escChar '\n' = "\\n"
    escChar '\t' = "\\t"
    escChar c    = T.singleton c
