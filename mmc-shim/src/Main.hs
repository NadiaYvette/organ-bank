-- | mmc-organ: Extract Mercury HLDS and emit OrganIR JSON.
--
-- Runs mmc --dump-hlds 50 on a Mercury source file, parses the dump,
-- and emits OrganIR JSON on stdout.

module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import qualified Data.Text.IO as TIO
import OrganBank.MmcShim (extractOrganIR)

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
      hPutStrLn stderr "Usage: mmc-organ <file.m>"
      hPutStrLn stderr "Extracts Mercury HLDS and emits OrganIR JSON on stdout."
      exitFailure
