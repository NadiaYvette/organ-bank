module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import qualified Data.Text.IO as TIO
import OrganBank.CShim (extractOrganIR)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputPath] -> do
      result <- extractOrganIR inputPath
      case result of
        Left err -> do
          hPutStrLn stderr $ "Error: " ++ err
          exitFailure
        Right json -> TIO.putStr json
    _ -> do
      hPutStrLn stderr "Usage: c-organ <file.c>"
      exitFailure
