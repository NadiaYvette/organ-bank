{- | Extract LLVM IR from C++ source via clang++ and emit OrganIR JSON.
Attempts to demangle C++ names via c++filt if available.
-}
module OrganBank.CppShim (extractOrganIR) where

import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName)
import System.Process (readProcessWithExitCode)

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
    (ec, out, err) <- readProcessWithExitCode "clang++" ["-emit-llvm", "-S", "-O2", "-o", "-", inputPath] ""
    case ec of
        ExitFailure _ -> return $ Left $ "clang++ failed: " ++ err
        ExitSuccess -> do
            let defs = parseLlvmIR (T.pack out)
            demangled <- mapM demangleFunc defs
            let modName = takeBaseName inputPath
                json = emitOrganIR modName inputPath demangled
            return $ Right json

data LlvmFunc = LlvmFunc
    { lfName :: Text
    , lfDemangled :: Text
    , lfRetTy :: Text
    , lfParams :: [(Text, Text)]
    , lfBlocks :: [(Text, [Text])]
    }
    deriving (Show)

demangleFunc :: LlvmFunc -> IO LlvmFunc
demangleFunc f = do
    (ec, out, _) <- readProcessWithExitCode "c++filt" [T.unpack (lfName f)] ""
    case ec of
        ExitSuccess -> return f{lfDemangled = T.strip (T.pack out)}
        _ -> return f{lfDemangled = lfName f}

parseLlvmIR :: Text -> [LlvmFunc]
parseLlvmIR ir = parseDefines (T.lines ir)

parseDefines :: [Text] -> [LlvmFunc]
parseDefines [] = []
parseDefines (l : ls)
    | "define " `T.isPrefixOf` l =
        let (bodyLines, rest) = collectBody ls 1
            func = parseDefine l bodyLines
         in func : parseDefines rest
    | otherwise = parseDefines ls

collectBody :: [Text] -> Int -> ([Text], [Text])
collectBody [] _ = ([], [])
collectBody (l : ls) depth
    | "}" `T.isSuffixOf` T.strip l && depth <= 1 = ([], ls)
    | "{" `T.isSuffixOf` T.strip l = let (b, r) = collectBody ls (depth + 1) in (l : b, r)
    | "}" `T.isSuffixOf` T.strip l = let (b, r) = collectBody ls (depth - 1) in (l : b, r)
    | otherwise = let (b, r) = collectBody ls depth in (l : b, r)

parseDefine :: Text -> [Text] -> LlvmFunc
parseDefine header bodyLines =
    let stripped = T.strip header
        afterDefine = T.drop 7 stripped
        afterLinkage = skipKws afterDefine
        (retTy, rest1) = T.breakOn " @" afterLinkage
        nameAndParams = T.drop 2 rest1
        (name_, rest2) = T.breakOn "(" nameAndParams
        paramStr = T.takeWhile (/= ')') (T.drop 1 rest2)
        params = parseParams paramStr
        blocks = parseBlocks bodyLines
     in LlvmFunc (T.strip name_) (T.strip name_) (T.strip retTy) params blocks

skipKws :: Text -> Text
skipKws t =
    let kws =
            [ "dso_local "
            , "internal "
            , "private "
            , "linkonce_odr "
            , "weak "
            , "hidden "
            , "noundef "
            , "signext "
            , "zeroext "
            ]
        go s = case filter (`T.isPrefixOf` s) kws of
            (k : _) -> go (T.drop (T.length k) s)
            [] -> s
     in go t

parseParams :: Text -> [(Text, Text)]
parseParams t
    | T.null (T.strip t) = []
    | otherwise = concatMap p1 (T.splitOn "," t)
  where
    p1 p =
        let ws = T.words (T.strip p)
         in case ws of
                [] -> []
                [x] -> [(x, "")]
                _ -> [(T.unwords (init ws), last ws)]

parseBlocks :: [Text] -> [(Text, [Text])]
parseBlocks = go "entry" []
  where
    go lbl acc [] = [(lbl, reverse acc)]
    go lbl acc (l : ls)
        | ":" `T.isSuffixOf` T.strip l && not (T.isPrefixOf "  " l) =
            (lbl, reverse acc) : go (T.strip (T.dropEnd 1 (T.strip l))) [] ls
        | otherwise =
            let trimmed = T.strip l
             in if T.null trimmed || ";" `T.isPrefixOf` trimmed
                    then go lbl acc ls
                    else go lbl (trimmed : acc) ls

-- | Emit OrganIR JSON using the organ-ir library.
emitOrganIR :: String -> FilePath -> [LlvmFunc] -> Text
emitOrganIR modName srcFile defs =
    renderOrganIR $
        IR.simpleOrganIR IR.LCpp "cpp-shim-0.1" (T.pack modName) srcFile (map funcToIR defs)

funcToIR :: LlvmFunc -> IR.Definition
funcToIR f =
    IR.Definition
        { IR.defName = IR.localName (lfDemangled f)
        , IR.defType =
            IR.TFn
                (map (\(ty, _) -> IR.FnArg Nothing (IR.tCon ty)) (lfParams f))
                IR.pureEffect
                (IR.tCon (lfRetTy f))
        , IR.defExpr = IR.EApp (IR.eVar "ssa_body") (map blockToIR (lfBlocks f))
        , IR.defSort = IR.SFun
        , IR.defVisibility = IR.Public
        , IR.defArity = length (lfParams f)
        }

blockToIR :: (Text, [Text]) -> IR.Expr
blockToIR (lbl, stmts) = IR.ETuple [IR.eString lbl, IR.EList (map IR.eString stmts)]
