module Main where

import OrganBank.JuliaShim (extractOrganIR)
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
          hPutStrLn stderr $ "julia-organ: " <> err
          exitFailure
        Right json -> TIO.putStr json
    _ -> do
      hPutStrLn stderr "Usage: julia-organ <file.jl>"
      hPutStrLn stderr "Extracts Julia typed IR and emits OrganIR JSON on stdout."
      exitFailure
