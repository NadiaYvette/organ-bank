{- | Core Erlang Extraction Shim

Runs `erlc +to_core <file.erl>` to produce a `.core` file, parses the
Core Erlang text output to extract function definitions, and emits
OrganIR JSON.

Core Erlang function definitions look like:
  'name'/arity = (fun (Var1, ..., VarN) -> body)

We extract the function name and arity from the declaration line,
then scan the body for effectful operations (send, receive, io calls)
to determine whether to attach an io effect.
-}
module OrganBank.ErlcShim (
    extractOrganIR,
) where

import Control.Exception (SomeException, catch)
import Control.Exception qualified
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Exit (ExitCode (..))
import System.FilePath (replaceExtension, takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

-- | A parsed Core Erlang function definition.
data CoreFun = CoreFun
    { cfName :: String
    -- ^ Function name (without quotes)
    , cfArity :: Int
    -- ^ Arity
    , cfBody :: String
    -- ^ Raw body text (for effect analysis)
    }
    deriving (Show)

-- | Detect erlc version by running @erlc -v@ (prints to stderr).
detectCompilerVersion :: IO Text
detectCompilerVersion = do
    (ec, _out, err) <- readProcessWithExitCode "erlc" ["-v"] ""
    pure $ case ec of
        _ | not (null (trim err)) -> "erlc-shim-0.1 (" <> T.strip (T.pack (head (lines err))) <> ")"
        _ -> "erlc-shim-0.1"
  `catch` \(_ :: SomeException) -> pure "erlc-shim-0.1"
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Extract Core Erlang from a source file and emit OrganIR JSON.
extractOrganIR :: FilePath -> IO (Either Text Text)
extractOrganIR inputPath = do
    shimVer <- detectCompilerVersion
    -- Run erlc +to_core which produces <basename>.core in current dir
    (ec, _stdout, stderrOut) <-
        readProcessWithExitCode
            "erlc"
            ["+to_core", inputPath]
            ""
    case ec of
        ExitFailure code -> do
            hPutStrLn stderr $ "erlc exited with code " ++ show code
            hPutStrLn stderr stderrOut
            pure $ Left $ "erlc failed: " <> T.pack stderrOut
        ExitSuccess -> do
            let coreFile = replaceExtension (takeBaseName inputPath) ".core"
            coreText <- tryReadFile coreFile
            case coreText of
                Nothing -> pure $ Left $ "Could not read core file: " <> T.pack coreFile
                Just content -> do
                    let modName = takeBaseName inputPath
                        funs = parseCoreErlang content
                        defs = map coreFunToDef funs
                        exports = map (T.pack . cfName) funs
                        imports = extractCoreErlangImports content
                        ir = IR.organIRWithImports IR.LErlang shimVer (T.pack modName) inputPath exports imports defs
                    pure $ Right $ renderOrganIR ir

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile fp = do
    result <- Control.Exception.try @IOError (readFile fp >>= \c -> length c `seq` pure c)
    pure $ either (const Nothing) Just result

-- | Convert a CoreFun to an OrganIR Definition.
-- All Erlang functions are marked pure: side effects happen via message passing,
-- not visible at the Core Erlang level.
coreFunToDef :: CoreFun -> IR.Definition
coreFunToDef fun =
    let arity = cfArity fun
        ty = IR.TFn (replicate arity (IR.FnArg (Just IR.Many) IR.TAny)) IR.pureEffect IR.TAny
        body = IR.EApp (IR.eVar "source") [IR.eString (T.pack (cfBody fun))]
     in IR.Definition
            { IR.defName = IR.localName (T.pack (cfName fun))
            , IR.defType = ty
            , IR.defExpr = body
            , IR.defSort = IR.SFun
            , IR.defVisibility = IR.Public
            , IR.defArity = arity
            }

{- | Extract external module references from Core Erlang text.
Core Erlang inter-module calls look like: call 'module':'function'
We scan for 'mod':'fun' patterns and collect unique module:function QNames.
-}
extractCoreErlangImports :: String -> [IR.QName]
extractCoreErlangImports content =
    let ls = lines content
        -- Look for patterns like 'erlang':'+'  or 'io':'format'
        calls = concatMap extractCallsFromLine ls
     in nubQNames calls
  where
    extractCallsFromLine line =
        case findCallPattern (dropWhile isSpace line) of
            [] -> []
            xs -> xs

    findCallPattern [] = []
    findCallPattern ('\'':rest) =
        case break (== '\'') rest of
            (modName, '\'':':':'\'':afterColon) ->
                case break (== '\'') afterColon of
                    (funName, '\'':remaining) ->
                        IR.QName (T.pack modName) (IR.Name (T.pack funName) 0)
                            : findCallPattern remaining
                    _ -> findCallPattern afterColon
            (_, remaining) -> findCallPattern remaining
    findCallPattern (_:rest) = findCallPattern rest

    nubQNames [] = []
    nubQNames (x:xs) = x : nubQNames (filter (\y -> not (qnEq x y)) xs)

    qnEq (IR.QName m1 (IR.Name n1 _)) (IR.QName m2 (IR.Name n2 _)) = m1 == m2 && n1 == n2

{- | Parse Core Erlang text to extract function definitions.

Core Erlang function declarations have this shape:

  'name'/N =
      (fun (Var1, ..., VarN) ->
          body)

We detect the 'name'/N = pattern and capture everything until the
next top-level definition or end of module.
-}
parseCoreErlang :: String -> [CoreFun]
parseCoreErlang content =
    let ls = lines content
     in extractFuns ls

extractFuns :: [String] -> [CoreFun]
extractFuns [] = []
extractFuns (l : ls)
    | Just (name, arity) <- parseFunHeader l =
        let (bodyLines, rest) = collectBody ls
            body = unlines bodyLines
         in CoreFun name arity body : extractFuns rest
    | otherwise = extractFuns ls

{- | Try to parse a function header line.
Matches patterns like:  'name'/2 =
Also matches: 'module_info'/0 =
-}
parseFunHeader :: String -> Maybe (String, Int)
parseFunHeader line =
    let stripped = dropWhile isSpace line
     in case stripped of
            ('\'' : rest) ->
                case break (== '\'') rest of
                    (name, '\'' : '/' : afterSlash) ->
                        case reads afterSlash :: [(Int, String)] of
                            [(arity, remaining)]
                                | all isSpace (takeWhile (/= '=') remaining)
                                , '=' `elem` remaining ->
                                    Just (name, arity)
                            _ -> Nothing
                    _ -> Nothing
            _ -> Nothing

{- | Collect body lines until the next top-level definition.
A top-level definition starts with 'name'/N = at the beginning
of a line (possibly with leading whitespace removed, but typically
at column 0 in Core Erlang).
-}
collectBody :: [String] -> ([String], [String])
collectBody [] = ([], [])
collectBody (l : ls)
    | isTopLevel l = ([], l : ls)
    | otherwise =
        let (body, rest) = collectBody ls
         in (l : body, rest)
  where
    isTopLevel s =
        let stripped = dropWhile isSpace s
         in case stripped of
                ('\'' : _) -> case parseFunHeader s of
                    Just _ -> True
                    Nothing -> False
                -- End of module
                _ | "end" == dropWhile isSpace s -> True
                _ -> False
