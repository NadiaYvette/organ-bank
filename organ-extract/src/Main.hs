module Main (main) where

import Data.List (isPrefixOf)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, stderr)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , waitForProcess
  )

-- | Map file extension to the organ-bank executable name.
extToExe :: String -> Maybe String
extToExe ext = case ext of
  ".hs"   -> Just "ghc-organ"
  ".rs"   -> Just "rustc-organ"
  ".m"    -> Just "mmc-organ"
  ".idr"  -> Just "idris2-organ"
  ".lean" -> Just "lean4-organ"
  ".erl"  -> Just "erlc-organ"
  ".purs" -> Just "purs-organ"
  ".ml"   -> Just "ocaml-organ"
  ".kk"   -> Just "koka-organ"
  ".swift" -> Just "swift-organ"
  ".agda" -> Just "agda-organ"
  ".fsx"  -> Just "fsharp-organ"
  ".scala" -> Just "scala3-organ"
  ".jl"   -> Just "julia-organ"
  ".zig"  -> Just "zig-organ"
  ".c"    -> Just "c-organ"
  ".cpp"  -> Just "cpp-organ"
  ".f90"  -> Just "fortran-organ"
  ".adb"  -> Just "ada-organ"
  ".sml"  -> Just "sml-organ"
  ".lisp" -> Just "cl-organ"
  ".scm"  -> Just "scheme-organ"
  ".pl"   -> Just "prolog-organ"
  ".lua"  -> Just "lua-organ-fe"
  ".fth"  -> Just "forth-organ-fe"
  _       -> Nothing

-- | All supported extensions for the help message.
supportedExts :: [String]
supportedExts =
  [ ".hs", ".rs", ".m", ".idr", ".lean", ".erl", ".purs", ".ml"
  , ".kk", ".swift", ".agda", ".fsx", ".scala", ".jl", ".zig"
  , ".c", ".cpp", ".f90", ".adb", ".sml", ".lisp", ".scm"
  , ".pl", ".lua", ".fth"
  ]

usage :: String -> String
usage prog = unlines
  [ "Usage: " ++ prog ++ " [--validate] [--pretty] <source-file>"
  , ""
  , "Flags:"
  , "  --validate   Pipe shim output through organ-validate"
  , "  --pretty     Pipe shim output through organ-validate --pretty"
  , ""
  , "Supported extensions:"
  , "  " ++ unwords supportedExts
  ]

data Opts = Opts
  { optValidate :: Bool
  , optPretty   :: Bool
  , optFile     :: Maybe FilePath
  }

parseArgs :: [String] -> Opts
parseArgs = go (Opts False False Nothing)
  where
    go opts [] = opts
    go opts ("--validate" : rest) = go opts { optValidate = True } rest
    go opts ("--pretty" : rest)   = go opts { optPretty = True } rest
    go opts ("--help" : _)        = opts { optFile = Nothing }
    go opts (arg : rest)
      | "--" `isPrefixOf` arg = go opts rest  -- skip unknown flags
      | otherwise = go opts { optFile = Just arg } rest

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  let opts = parseArgs args
  case optFile opts of
    Nothing -> do
      hPutStrLn stderr (usage prog)
      exitFailure
    Just srcFile -> do
      let ext = takeExtension srcFile
      case extToExe ext of
        Nothing -> do
          hPutStrLn stderr $ "Error: unrecognized extension " ++ show ext
          hPutStrLn stderr $ "Supported: " ++ unwords supportedExts
          exitFailure
        Just exe -> do
          let wantPipe = optValidate opts || optPretty opts
              shimProc = (proc exe [srcFile])
                { std_out = if wantPipe then CreatePipe else Inherit }
          (_, mShimOut, _, shimPh) <- createProcess shimProc
          case mShimOut of
            Nothing -> do
              -- No pipe: just wait for the shim.
              code <- waitForProcess shimPh
              exitWith code
            Just shimOut -> do
              -- Pipe shim stdout into organ-validate stdin.
              let valArgs = if optPretty opts then ["--pretty"] else []
                  valProc = (proc "organ-validate" valArgs)
                    { std_in = UseHandle shimOut }
              (_, _, _, valPh) <- createProcess valProc
              shimCode <- waitForProcess shimPh
              valCode  <- waitForProcess valPh
              case (shimCode, valCode) of
                (ExitSuccess, ExitSuccess) -> pure ()
                (ExitFailure _, _)         -> exitWith shimCode
                (_, _)                     -> exitWith valCode
