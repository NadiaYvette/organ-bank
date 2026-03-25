-- | Extract GCC GIMPLE from Fortran source via gfortran and emit OrganIR JSON.
module OrganBank.FortranShim (extractOrganIR) where

import Control.Exception (SomeException, catch)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Directory (createDirectoryIfMissing, listDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, (</>))
import System.Process (readProcessWithExitCode)

-- | Detect gfortran version by running @gfortran --version@.
detectCompilerVersion :: IO Text
detectCompilerVersion = do
    (ec, out, _) <- readProcessWithExitCode "gfortran" ["--version"] ""
    pure $ case ec of
        ExitSuccess | (l : _) <- lines out -> "fortran-shim-0.1 (" <> T.strip (T.pack l) <> ")"
        _ -> "fortran-shim-0.1"
  `catch` \(_ :: SomeException) -> pure "fortran-shim-0.1"

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
    shimVer <- detectCompilerVersion
    let tmpDir = "/tmp/organ-bank-fortran-" ++ show (hash inputPath)
    createDirectoryIfMissing True tmpDir
    (ec, _out, err) <-
        readProcessWithExitCode
            "gfortran"
            ["-fdump-tree-optimized", "-O2", "-c", inputPath, "-o", tmpDir </> "out.o"]
            ""
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
                        ir = IR.simpleOrganIR IR.LFortran shimVer (T.pack modName) inputPath (map funcToIR defs)
                    cleanup tmpDir
                    return $ Right (renderOrganIR ir)

hash :: String -> Int
hash = foldl (\h c -> h * 31 + fromEnum c) 0

cleanup :: FilePath -> IO ()
cleanup dir = removeDirectoryRecursive dir `catch` (\(_ :: SomeException) -> return ())

findDumpDir :: FilePath -> FilePath -> IO FilePath
findDumpDir inputPath _tmpDir = do
    -- gfortran puts dump files next to the source file
    let srcDir = case reverse (dropWhile (/= '/') (reverse inputPath)) of
            "" -> "."
            d -> init d -- drop trailing /
    return srcDir

findOptimizedFile :: FilePath -> IO (Maybe FilePath)
findOptimizedFile dir = do
    files <- listDirectory dir
    let opts = filter (\f -> ".optimized" `isSuffix` f) files
    case opts of
        (f : _) -> return $ Just (dir </> f)
        [] -> return Nothing
  where
    isSuffix suf s = suf == reverse (take (length suf) (reverse s))

data GimpleFunc = GimpleFunc
    { gfName :: Text
    , gfRetTy :: Text
    , gfParamTys :: [(Text, Text)] -- (type, name)
    , gfBlocks :: [(Text, [Text])]
    }
    deriving (Show)

parseGimple :: Text -> [GimpleFunc]
parseGimple content = parseFunctions (T.lines content)

parseFunctions :: [Text] -> [GimpleFunc]
parseFunctions [] = []
parseFunctions (l : ls)
    | ";; Function " `T.isPrefixOf` l =
        let name = extractFuncName l
            -- The next non-empty line before '{' is the C-like signature
            (sigLines, bodyLs) = span (\x -> not ("{" `T.isSuffixOf` T.strip x)) ls
            bodyLs' = drop 1 bodyLs -- skip the '{' line
            sig = T.strip (T.unwords (map T.strip sigLines))
            (retTy, paramTys) = parseGimpleSig sig
            (bodyLines, rest) = collectGimpleBody bodyLs' 1
            blocks = parseGimpleBlocks bodyLines
         in GimpleFunc name retTy paramTys blocks : parseFunctions rest
    | otherwise = parseFunctions ls

extractFuncName :: Text -> Text
extractFuncName l =
    let afterPrefix = T.drop 14 l -- drop ";; Function "
        name = T.takeWhile (\c -> c /= ' ' && c /= '(') afterPrefix
     in name

collectGimpleBody :: [Text] -> Int -> ([Text], [Text])
collectGimpleBody [] _ = ([], [])
collectGimpleBody (l : ls) depth
    | "{" `T.isSuffixOf` T.strip l =
        let (b, r) = collectGimpleBody ls (depth + 1) in (l : b, r)
    | "}" `T.isSuffixOf` T.strip l =
        if depth <= 1
            then ([], ls)
            else let (b, r) = collectGimpleBody ls (depth - 1) in (l : b, r)
    | otherwise = let (b, r) = collectGimpleBody ls depth in (l : b, r)

parseGimpleBlocks :: [Text] -> [(Text, [Text])]
parseGimpleBlocks = go "entry" []
  where
    go lbl acc [] = [(lbl, reverse acc) | not (null acc)]
    go lbl acc (l : ls)
        | "  <bb " `T.isPrefixOf` l =
            let newLbl = T.strip (T.takeWhile (/= '>') (T.drop 5 l))
                prev = [(lbl, reverse acc) | not (null acc)]
             in prev ++ go (T.concat ["bb_", newLbl]) [] ls
        | otherwise =
            let trimmed = T.strip l
             in if T.null trimmed
                    then go lbl acc ls
                    else go lbl (trimmed : acc) ls

-- | Parse a GIMPLE C-like function signature.
--   e.g. "integer(kind=4) factorial_ (integer(kind=4) n)"
--   Returns (returnType, [(paramType, paramName)])
parseGimpleSig :: Text -> (Text, [(Text, Text)])
parseGimpleSig sig
    | T.null sig = ("void", [])
    | otherwise =
        -- Find the function name by looking for " name_ (" or " name ("
        let -- Split at the opening parenthesis of the param list
            (beforeParams, rest) = T.breakOn " (" sig
            -- The return type is everything before the last word (function name)
            ws = T.words beforeParams
            (retTyWords, _nameWords) = case ws of
                [] -> ([], [])
                _ -> (init ws, [last ws])
            retTy = if null retTyWords then "void" else T.unwords retTyWords
            -- Parse parameters from between ( and )
            paramStr =
                if T.null rest
                    then ""
                    else T.takeWhile (/= ')') (T.drop 2 rest) -- drop " ("
            params = parseGimpleParams paramStr
         in (retTy, params)

-- | Parse GIMPLE parameter list like "integer(kind=4) n, real(kind=8) x"
parseGimpleParams :: Text -> [(Text, Text)]
parseGimpleParams t
    | T.null (T.strip t) = []
    | otherwise = concatMap parseOneParam (splitGimpleParams t)
  where
    parseOneParam p =
        let ws = T.words (T.strip p)
         in case ws of
                [] -> []
                [_] -> [(T.strip p, "")]
                _ -> [(T.unwords (init ws), last ws)]

-- | Split GIMPLE params on commas, respecting parentheses (for types like integer(kind=4)).
splitGimpleParams :: Text -> [Text]
splitGimpleParams = go (0 :: Int) "" . T.unpack
  where
    go _ acc [] = [T.pack (reverse acc)]
    go depth acc (c : cs)
        | c == '(' = go (depth + 1) (c : acc) cs
        | c == ')' = go (max 0 (depth - 1)) (c : acc) cs
        | c == ',' && depth == 0 = T.pack (reverse acc) : go 0 "" cs
        | otherwise = go depth (c : acc) cs

-- | Map a GIMPLE type string to an OrganIR type.
gimpleTypeToIR :: Text -> IR.Ty
gimpleTypeToIR t = case T.strip t of
    "void" -> IR.tCon "Void"
    s
        | "integer(kind=4)" `T.isPrefixOf` s -> IR.tCon "Int32"
        | "integer(kind=8)" `T.isPrefixOf` s -> IR.tCon "Int64"
        | "integer(kind=2)" `T.isPrefixOf` s -> IR.tCon "Int16"
        | "integer(kind=1)" `T.isPrefixOf` s -> IR.tCon "Int8"
        | "real(kind=4)" `T.isPrefixOf` s -> IR.tCon "Float32"
        | "real(kind=8)" `T.isPrefixOf` s -> IR.tCon "Float64"
        | "real(kind=16)" `T.isPrefixOf` s -> IR.tCon "Float128"
        | "logical(kind=4)" `T.isPrefixOf` s -> IR.tCon "Bool"
        | "character(kind=1)" `T.isPrefixOf` s -> IR.tCon "Char"
        | "complex(kind=" `T.isPrefixOf` s -> IR.tCon "Complex"
        | "integer(kind=" `T.isPrefixOf` s -> IR.tCon "Int"
        | "real(kind=" `T.isPrefixOf` s -> IR.tCon "Float"
        | "*" `T.isSuffixOf` s -> IR.tCon "Ptr"
        | otherwise -> IR.tCon s

-- | Convert a parsed GIMPLE function to an OrganIR definition.
funcToIR :: GimpleFunc -> IR.Definition
funcToIR f =
    IR.Definition
        { IR.defName = IR.localName (gfName f)
        , IR.defType =
            IR.TFn
                (map (\(ty, _) -> IR.FnArg Nothing (gimpleTypeToIR ty)) (gfParamTys f))
                IR.pureEffect
                (gimpleTypeToIR (gfRetTy f))
        , IR.defExpr = IR.EApp (IR.eVar "gimple_body") (map blockToIR (gfBlocks f))
        , IR.defSort = IR.SFun
        , IR.defVisibility = IR.Public
        , IR.defArity = length (gfParamTys f)
        }

-- | Convert a GIMPLE basic block to an OrganIR expression.
blockToIR :: (Text, [Text]) -> IR.Expr
blockToIR (lbl, stmts) = IR.ETuple [IR.eString lbl, IR.EList (map IR.eString stmts)]
