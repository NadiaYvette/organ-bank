module Main (main) where

import Data.Text.IO qualified as TIO
import ErlangFrontend.Lexer (lexErlang)
import ErlangFrontend.Parser (parseForms)
import ErlangFrontend.ToOrganIR (emitErlangIR)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fp] -> do
            src <- TIO.readFile fp
            case lexErlang src of
                Left err -> putStrLn $ "Lex error: " ++ err
                Right toks -> case parseForms toks of
                    Left err -> putStrLn $ "Parse error: " ++ err
                    Right forms -> TIO.putStrLn (emitErlangIR (takeBaseName fp) fp forms)
        _ -> putStrLn "Usage: erlang-organ-fe <file.erl>"
