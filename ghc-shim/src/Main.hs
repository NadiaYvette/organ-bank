module Main where

import Data.Text.IO qualified as TIO
import OrganBank.GhcShim (extractOrganIR)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)

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
