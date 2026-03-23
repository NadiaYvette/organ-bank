module Main where

import OrganBank.PursShim (extractOrganIR)
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> do
      result <- extractOrganIR inputFile
      case result of
        Left err -> do
          hPutStrLn stderr $ "purs-organ: " <> err
          exitFailure
        Right json -> TIO.putStrLn json
    _ -> do
      hPutStrLn stderr "Usage: purs-organ <file.purs>"
      hPutStrLn stderr "Extracts PureScript CoreFn and emits OrganIR JSON on stdout."
      exitFailure
