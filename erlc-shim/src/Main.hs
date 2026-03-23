-- | erlc-organ: Extract Core Erlang and emit OrganIR JSON.
--
-- Runs erlc +to_core on an Erlang source file, parses the Core Erlang
-- output, and emits OrganIR JSON on stdout.

module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import qualified Data.Text.IO as TIO
import OrganBank.ErlcShim (extractOrganIR)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputPath] -> do
      result <- extractOrganIR inputPath
      case result of
        Left err -> do
          hPutStrLn stderr $ "Error: " ++ show err
          exitFailure
        Right json -> TIO.putStr json
    _ -> do
      hPutStrLn stderr "Usage: erlc-organ <file.erl>"
      hPutStrLn stderr "Extracts Core Erlang and emits OrganIR JSON on stdout."
      exitFailure
