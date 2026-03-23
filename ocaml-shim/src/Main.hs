module Main where

import OrganBank.OcamlShim (extractOrganIR)
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
          hPutStrLn stderr $ "ocaml-organ: " <> err
          exitFailure
        Right json -> TIO.putStrLn json
    _ -> do
      hPutStrLn stderr "Usage: ocaml-organ <file.ml>"
      hPutStrLn stderr "Extracts OCaml Lambda IR and emits OrganIR JSON on stdout."
      exitFailure
