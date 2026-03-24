module Main (main) where

import Data.Text.IO qualified as TIO
import LuaFrontend.Lexer (lexLua)
import LuaFrontend.Parser (parseLua)
import LuaFrontend.ToOrganIR (emitLuaIR)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fp] -> do
            src <- TIO.readFile fp
            case lexLua src of
                Left err -> putStrLn $ "Lex error: " ++ err
                Right toks -> case parseLua toks of
                    Left err -> putStrLn $ "Parse error: " ++ err
                    Right block -> TIO.putStrLn (emitLuaIR (takeBaseName fp) fp block)
        _ -> putStrLn "Usage: lua-organ-fe <file.lua>"
