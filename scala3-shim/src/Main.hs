module Main where

import OrganBank.Scala3Shim (extractOrganIR)
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
          TIO.hPutStrLn stderr $ "scala3-organ: " <> err
          exitFailure
        Right json -> TIO.putStrLn json
    _ -> do
      putStrLn "Usage: scala3-organ <file.scala>"
      putStrLn "Extracts Scala 3 post-erasure trees and emits OrganIR JSON on stdout."
      exitFailure
