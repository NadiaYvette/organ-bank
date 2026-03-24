-- | Round-trip test: render → parse → render, check equality.
module Main (main) where

import Data.Text.IO qualified as TIO
import OrganIR.Json (renderOrganIR)
import OrganIR.Parse (parseOrganIR)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Usage: roundtrip-test <file.json>"
            exitFailure
        (f : _) -> do
            txt <- TIO.readFile f
            case parseOrganIR txt of
                Left err -> do
                    putStrLn $ "Parse failed: " ++ show err
                    exitFailure
                Right ir -> do
                    let rendered = renderOrganIR ir
                    case parseOrganIR rendered of
                        Left err2 -> do
                            putStrLn $ "Re-parse failed: " ++ show err2
                            exitFailure
                        Right ir2 -> do
                            let rendered2 = renderOrganIR ir2
                            if rendered == rendered2
                                then putStrLn "PASS: round-trip stable"
                                else do
                                    putStrLn "FAIL: render mismatch after round-trip"
                                    putStrLn $ "First:  " ++ show (take 200 (show rendered))
                                    putStrLn $ "Second: " ++ show (take 200 (show rendered2))
                                    exitFailure
