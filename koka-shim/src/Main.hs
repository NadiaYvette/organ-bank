module Main (main) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import OrganBank.KokaShim (extractOrganIR)
import Data.Text.IO qualified as TIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file]
      | ".kk" `isSuffixOf` file -> do
          result <- extractOrganIR file
          TIO.putStrLn result
      | otherwise -> die "Input file must have .kk extension"
    _ -> do
      prog <- getProgName
      die $ "Usage: " ++ prog ++ " <file.kk>"
  where
    isSuffixOf suffix str = drop (length str - length suffix) str == suffix
    die msg = hPutStrLn stderr msg >> exitFailure
