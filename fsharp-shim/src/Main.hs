module Main where

import OrganBank.FSharpShim (extractOrganIR)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> do
      result <- extractOrganIR inputFile
      case result of
        Left err -> do
          hPutStrLn stderr $ "fsharp-organ: " <> err
          exitFailure
        Right json -> TIO.putStrLn json
    _ -> do
      hPutStrLn stderr "Usage: fsharp-organ <file.fsx|file.fs>"
      hPutStrLn stderr "Extracts F# typed AST and emits OrganIR JSON on stdout."
      exitFailure
