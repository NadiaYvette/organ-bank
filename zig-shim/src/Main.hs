module Main where

import OrganBank.ZigShim (extractOrganIR)
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
          hPutStrLn stderr $ "zig-organ: " <> err
          exitFailure
        Right json -> TIO.putStr json
    _ -> do
      putStrLn "Usage: zig-organ <file.zig>"
      putStrLn "Extracts Zig ZIR (AstGen output) and emits OrganIR JSON on stdout."
      putStrLn ""
      putStrLn "Note: ZIR is untyped, pre-semantic-analysis IR."
      putStrLn "The zig-tool/zir-extract binary must be built first:"
      putStrLn "  cd zig-tool && zig build"
      exitFailure
