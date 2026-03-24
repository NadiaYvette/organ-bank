-- | prolog-organ-fe: Independent ISO Prolog frontend emitting OrganIR JSON.
module Main (main) where

import Data.Text.IO qualified as TIO
import PrologFrontend.Lexer (lexProlog)
import PrologFrontend.Parser (parseSentences)
import PrologFrontend.ToOrganIR (emitPrologIR)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main =
    getArgs >>= \case
        [path] -> run path
        _ -> do
            hPutStrLn stderr "Usage: prolog-organ-fe <file.pl>"
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
        tokens <- lexProlog src
        sentences <- parseSentences tokens
        Right (emitPrologIR (takeBaseName p) p sentences)
