-- | scheme-organ-fe: Independent R7RS-small Scheme frontend emitting OrganIR JSON.
module Main (main) where

import Data.Text.IO qualified as TIO
import SchemeFrontend.Desugar (desugarProgram)
import SchemeFrontend.Lexer (lexScheme)
import SchemeFrontend.Reader (readDatums)
import SchemeFrontend.ToOrganIR (emitSchemeIR)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main =
    getArgs >>= \case
        [path] -> run path
        _ -> do
            hPutStrLn stderr "Usage: scheme-organ-fe <file.scm>"
            exitFailure

run :: FilePath -> IO ()
run path = do
    src <- TIO.readFile path
    case pipeline path src of
        Left err -> do
            hPutStrLn stderr $ "Error: " ++ err
            exitFailure
        Right json -> TIO.putStr json
  where
    pipeline p src = do
        tokens <- lexScheme src
        datums <- readDatums tokens
        tops <- desugarProgram datums
        Right (emitSchemeIR (takeBaseName p) p tops)
