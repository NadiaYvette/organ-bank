-- | Extract LLVM IR from C++ source via clang++ and emit OrganIR JSON.
-- Attempts to demangle C++ names via c++filt if available.
module OrganBank.CppShim (extractOrganIR) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
  (ec, out, err) <- readProcessWithExitCode "clang++"
    ["-emit-llvm", "-S", "-O2", "-o", "-", inputPath] ""
  case ec of
    ExitFailure _ -> return $ Left $ "clang++ failed: " ++ err
    ExitSuccess -> do
      let defs = parseLlvmIR (T.pack out)
      demangled <- mapM demangleFunc defs
      let modName = takeBaseName inputPath
          json = emitOrganIR modName inputPath demangled
      return $ Right json

takeBaseName :: FilePath -> String
takeBaseName = reverse . takeWhile (/= '/') . reverse

data LlvmFunc = LlvmFunc
  { lfName    :: Text
  , lfDemangled :: Text
  , lfRetTy   :: Text
  , lfParams  :: [(Text, Text)]
  , lfBlocks  :: [(Text, [Text])]
  } deriving (Show)

demangleFunc :: LlvmFunc -> IO LlvmFunc
demangleFunc f = do
  (ec, out, _) <- readProcessWithExitCode "c++filt" [T.unpack (lfName f)] ""
  case ec of
    ExitSuccess -> return f { lfDemangled = T.strip (T.pack out) }
    _ -> return f { lfDemangled = lfName f }

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
      afterDefine = T.drop 7 stripped
      afterLinkage = skipKws afterDefine
      (retTy, rest1) = T.breakOn " @" afterLinkage
      nameAndParams = T.drop 2 rest1
      (name, rest2) = T.breakOn "(" nameAndParams
      paramStr = T.takeWhile (/= ')') (T.drop 1 rest2)
      params = parseParams paramStr
      blocks = parseBlocks bodyLines
  in LlvmFunc (T.strip name) (T.strip name) (T.strip retTy) params blocks

skipKws :: Text -> Text
skipKws t =
  let kws = ["dso_local ", "internal ", "private ", "linkonce_odr ", "weak ", "hidden ",
             "noundef ", "signext ", "zeroext "]
      go s = case filter (`T.isPrefixOf` s) kws of
               (k:_) -> go (T.drop (T.length k) s)
               []    -> s
  in go t

parseParams :: Text -> [(Text, Text)]
parseParams t
  | T.null (T.strip t) = []
  | otherwise = concatMap p1 (T.splitOn "," t)
  where
    p1 p = let ws = T.words (T.strip p)
           in case ws of
                []  -> []
                [x] -> [(x, "")]
                _   -> [(T.unwords (init ws), last ws)]

parseBlocks :: [Text] -> [(Text, [Text])]
parseBlocks = go "entry" []
  where
    go lbl acc [] = [(lbl, reverse acc)]
    go lbl acc (l:ls)
      | ":" `T.isSuffixOf` T.strip l && not (T.isPrefixOf "  " l) =
        (lbl, reverse acc) : go (T.strip (T.dropEnd 1 (T.strip l))) [] ls
      | otherwise =
        let trimmed = T.strip l
        in if T.null trimmed || ";" `T.isPrefixOf` trimmed
           then go lbl acc ls
           else go lbl (trimmed : acc) ls

emitOrganIR :: String -> FilePath -> [LlvmFunc] -> Text
emitOrganIR modName srcFile defs = T.concat
  [ "{\n  \"source_language\": \"cpp\",\n"
  , "  \"module_name\": ", jsonStr (T.pack modName), ",\n"
  , "  \"source_file\": ", jsonStr (T.pack srcFile), ",\n"
  , "  \"compiler_version\": \"clang++-llvm\",\n"
  , "  \"definitions\": [\n"
  , T.intercalate ",\n" (map emitDef defs)
  , "\n  ]\n}\n"
  ]

emitDef :: LlvmFunc -> Text
emitDef f = T.concat
  [ "    {\n"
  , "      \"name\": {\"module\": \"\", \"text\": ", jsonStr (lfDemangled f)
  , ", \"mangled\": ", jsonStr (lfName f), ", \"unique\": 0},\n"
  , "      \"type\": {\"tag\": \"fn\", \"params\": ["
  , T.intercalate ", " [T.concat ["{\"tag\": \"con\", \"name\": ", jsonStr ty, "}"] | (ty, _) <- lfParams f]
  , "], \"result\": {\"tag\": \"con\", \"name\": ", jsonStr (lfRetTy f), "}},\n"
  , "      \"expr\": {\"tag\": \"ssa_body\", \"blocks\": [\n"
  , T.intercalate ",\n" (map emitBlock (lfBlocks f))
  , "\n      ]},\n"
  , "      \"sort\": \"fun\",\n      \"visibility\": \"public\",\n"
  , "      \"arity\": ", T.pack (show (length (lfParams f))), "\n    }"
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
