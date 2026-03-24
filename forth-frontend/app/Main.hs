module Main (main) where

import Data.Text.IO qualified as TIO
import ForthFrontend.Lexer (lexForth)
import ForthFrontend.Parser (parseForth)
import ForthFrontend.ToOrganIR (emitForthIR)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fp] -> do
            src <- TIO.readFile fp
            case lexForth src of
                Left err -> putStrLn $ "Lex error: " ++ err
                Right toks -> case parseForth toks of
                    Left err -> putStrLn $ "Parse error: " ++ err
                    Right items -> TIO.putStrLn (emitForthIR (takeBaseName fp) fp items)
        _ -> putStrLn "Usage: forth-organ-fe <file.fs>"
