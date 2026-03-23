-- | agda-organ: Extract Agda Treeless IR (via MAlonzo) and emit OrganIR JSON.

module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import qualified Data.Text.IO as TIO
import OrganBank.AgdaShim (extractOrganIR)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputPath] -> do
      result <- extractOrganIR inputPath
      case result of
        Left err -> do
          hPutStrLn stderr $ "agda-organ: " ++ show err
          exitFailure
        Right json -> TIO.putStr json
    _ -> do
      hPutStrLn stderr "Usage: agda-organ <file.agda>"
      hPutStrLn stderr "Extracts Agda Treeless IR (via MAlonzo) and emits OrganIR JSON on stdout."
      exitFailure
