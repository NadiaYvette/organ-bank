module Main where

import OrganBank.SwiftShim (extractOrganIR)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> do
      result <- extractOrganIR inputFile
      case result of
        Left err -> do
          TIO.hPutStrLn stderr $ "swift-organ: " <> err
          exitFailure
        Right json -> TIO.putStrLn json
    _ -> do
      putStrLn "Usage: swift-organ <file.swift>"
      putStrLn "Extracts Swift SIL and emits OrganIR JSON on stdout."
      exitFailure
