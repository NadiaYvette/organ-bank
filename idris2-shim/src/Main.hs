-- | idris2-organ: Extract Idris 2 case trees and emit OrganIR JSON.
--
-- Runs idris2 --dumpcases on an Idris 2 source file, parses the output,
-- and emits OrganIR JSON on stdout.

module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import qualified Data.Text.IO as TIO
import OrganBank.Idris2Shim (extractOrganIR)

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
      hPutStrLn stderr "Usage: idris2-organ <file.idr>"
      hPutStrLn stderr "Extracts Idris 2 case trees and emits OrganIR JSON on stdout."
      exitFailure
