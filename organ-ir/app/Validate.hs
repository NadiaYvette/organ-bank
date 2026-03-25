module Main (main) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OrganIR.Binary (decodeOrganIR)
import OrganIR.Json (renderOrganIR)
import OrganIR.Parse (parseJSON, parseOrganIR)
import OrganIR.Pretty (ppOrganIR)
import OrganIR.Schema (SchemaError (..), schemaCheck)
import OrganIR.Validate (Severity (..), Warning (..), validateOrganIR)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    let (flags, files) = partition' isFlag args
        pretty = "--pretty" `elem` flags
        schema = "--schema" `elem` flags
        binary = "--binary" `elem` flags
    if binary
        then do
            input <- case files of
                [] -> BS.getContents
                [f] -> BS.readFile f
                _ -> do
                    hPutStrLn stderr "Usage: organ-validate [--pretty] [--schema] [--binary] [file]"
                    exitFailure
            runBinaryValidate pretty input
        else do
            input <- case files of
                [] -> TIO.getContents
                [f] -> TIO.readFile f
                _ -> do
                    hPutStrLn stderr "Usage: organ-validate [--pretty] [--schema] [--binary] [file.json]"
                    exitFailure
            if schema
                then runSchemaCheck input
                else runValidate pretty input

runBinaryValidate :: Bool -> BS.ByteString -> IO ()
runBinaryValidate pretty input = case decodeOrganIR input of
    Left err -> do
        hPutStrLn stderr $ "Binary decode error: " ++ err
        exitFailure
    Right ir -> do
        -- Output the JSON representation
        let json = renderOrganIR ir
        TIO.putStr json
        TIO.putStr "\n"
        -- Validate
        let ws = validateOrganIR ir
        when pretty (TIO.putStr (ppOrganIR ir))
        mapM_ (hPutStrLn stderr . fmtWarning) ws
        if any (\w -> wSeverity w == Error) ws
            then exitFailure
            else exitSuccess

runSchemaCheck :: T.Text -> IO ()
runSchemaCheck input = case parseJSON input of
    Left err -> do
        hPutStrLn stderr $ "JSON parse error: " ++ T.unpack err
        exitFailure
    Right jv -> do
        let errs = schemaCheck jv
        mapM_ (hPutStrLn stderr . fmtSchemaError) errs
        if null errs
            then do
                hPutStrLn stderr "Schema check passed."
                exitSuccess
            else do
                hPutStrLn stderr $
                    show (length errs) ++ " schema violation(s) found."
                exitFailure

runValidate :: Bool -> T.Text -> IO ()
runValidate pretty input = case parseOrganIR input of
    Left err -> do
        hPutStrLn stderr $ "Parse error: " ++ T.unpack err
        exitFailure
    Right ir -> do
        let ws = validateOrganIR ir
        when pretty (TIO.putStr (ppOrganIR ir))
        mapM_ (hPutStrLn stderr . fmtWarning) ws
        if any (\w -> wSeverity w == Error) ws
            then exitFailure
            else exitSuccess

isFlag :: String -> Bool
isFlag ('-' : '-' : _) = True
isFlag _ = False

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' _ [] = ([], [])
partition' p (x : xs)
    | p x = (x : yes, no)
    | otherwise = (yes, x : no)
  where
    (yes, no) = partition' p xs

when :: Bool -> IO () -> IO ()
when True act = act
when False _ = pure ()

fmtWarning :: Warning -> String
fmtWarning w =
    "[" ++ show (wSeverity w) ++ "] " ++ T.unpack (wPath w) ++ ": " ++ T.unpack (wMessage w)

fmtSchemaError :: SchemaError -> String
fmtSchemaError e =
    "[SCHEMA] " ++ T.unpack (sePath e) ++ ": " ++ T.unpack (seMessage e)
