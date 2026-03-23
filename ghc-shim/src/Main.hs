module Main where

import OrganBank.GhcShim (extractOrganIR)
import System.Environment (getArgs)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile] -> do
      result <- extractOrganIR inputFile
      case result of
        Left err -> do
          TIO.hPutStrLn stderr $ "ghc-organ: " <> err
          exitFailure
        Right json -> TIO.putStrLn json
    _ -> do
      putStrLn "Usage: ghc-organ <file.hs>"
      putStrLn "Extracts GHC Core and emits OrganIR JSON on stdout."
      exitFailure
  where
    stderr = undefined -- will be System.IO.stderr
    exitFailure = undefined -- will be System.Exit.exitFailure
