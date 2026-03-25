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

import Control.Exception qualified
import Data.Char (isSpace)
import Data.List (isInfixOf)
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

-- | Extract Core Erlang from a source file and emit OrganIR JSON.
extractOrganIR :: FilePath -> IO (Either Text Text)
extractOrganIR inputPath = do
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
                        ir = IR.organIRWithExports IR.LErlang "erlc-shim-0.1" (T.pack modName) inputPath exports defs
                    pure $ Right $ renderOrganIR ir

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile fp = do
    result <- Control.Exception.try @IOError (readFile fp >>= \c -> length c `seq` pure c)
    pure $ either (const Nothing) Just result

-- | Convert a CoreFun to an OrganIR Definition.
coreFunToDef :: CoreFun -> IR.Definition
coreFunToDef fun =
    let arity = cfArity fun
        effectRow
            | isEffectful (cfBody fun) =
                IR.EffectRow [IR.qualName "std" "io"] Nothing
            | otherwise = IR.pureEffect
        ty = IR.TFn (replicate arity (IR.FnArg (Just IR.Many) IR.TAny)) effectRow IR.TAny
        body = IR.EApp (IR.eVar "source") [IR.eString (T.pack (cfBody fun))]
     in IR.Definition
            { IR.defName = IR.localName (T.pack (cfName fun))
            , IR.defType = ty
            , IR.defExpr = body
            , IR.defSort = IR.SFun
            , IR.defVisibility = IR.Public
            , IR.defArity = arity
            }

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

{- | Determine if a function body looks effectful.
In Erlang, most functions are effectful. We mark as "pure" only
functions whose body does not contain obvious IO indicators.
-}
isEffectful :: String -> Bool
isEffectful body = any (`isInfixOf` body) ioIndicators
  where
    ioIndicators =
        [ "call 'erlang':'send'"
        , "call 'erlang':'!':"
        , "'receive'"
        , "call 'io':"
        , "call 'file':"
        , "call 'gen_server':"
        , "call 'gen_statem':"
        , "call 'ets':"
        , "call 'dets':"
        , "call 'mnesia':"
        , "call 'erlang':'put'"
        , "call 'erlang':'get'"
        , "call 'erlang':'erase'"
        , "call 'erlang':'self'"
        , "call 'erlang':'spawn'"
        , "call 'erlang':'register'"
        , "call 'erlang':'process_flag'"
        , "call 'erlang':'port_command'"
        , "call 'erlang':'open_port'"
        , "call 'erlang':'halt'"
        , "call 'erlang':'exit'"
        , "call 'erlang':'throw'"
        , "call 'erlang':'error'"
        , "call 'erlang':'display'"
        , "receive"
        , "primop" -- many primops are effectful
        ]
