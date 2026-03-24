-- | sml-organ-fe: Independent Standard ML frontend emitting OrganIR JSON.
module Main (main) where

import Data.Text (Text)
import Data.Text.IO qualified as TIO
import SmlFrontend.Basis (initialConEnv, initialEnv)
import SmlFrontend.Elab.Infer (inferProgram, initInferState)
import SmlFrontend.Lexer (lexSml)
import SmlFrontend.Parser (parseSml)
import SmlFrontend.ToOrganIR (emitOrganIR)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> run path
        _ -> do
            hPutStrLn stderr "Usage: sml-organ-fe <file.sml>"
            exitFailure

run :: FilePath -> IO ()
run path = do
    src <- TIO.readFile path
    case pipeline path src of
        Left err -> do
            hPutStrLn stderr $ "Error: " ++ err
            exitFailure
        Right json -> TIO.putStr json

pipeline :: FilePath -> Text -> Either String Text
pipeline path src = do
    tokens <- lexSml src
    ast <- parseSml tokens
    let st0 = initInferState initialEnv initialConEnv
    st <- inferProgram st0 ast
    let modName = takeBaseName path
    Right (emitOrganIR modName path ast st)
