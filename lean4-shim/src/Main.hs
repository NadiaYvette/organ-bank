module Main (main) where

import Data.Text.IO qualified as TIO
import OrganBank.Lean4Shim (extractOrganIR)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file]
            | ".lean" `isSuffixOf` file -> do
                result <- extractOrganIR file
                case result of
                    Right json -> TIO.putStrLn json
                    Left err -> do
                        TIO.hPutStrLn stderr err
                        exitFailure
            | otherwise -> die "Input file must have .lean extension"
        _ -> do
            prog <- getProgName
            die $ "Usage: " ++ prog ++ " <file.lean>"
  where
    isSuffixOf :: String -> String -> Bool
    isSuffixOf suffix str = drop (length str - length suffix) str == suffix
    die :: String -> IO ()
    die msg = hPutStrLn stderr msg >> exitFailure
