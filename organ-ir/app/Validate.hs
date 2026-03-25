module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OrganIR.Parse (parseOrganIR)
import OrganIR.Pretty (ppOrganIR)
import OrganIR.Validate (Severity (..), Warning (..), validateOrganIR)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    let (flags, files) = partition' isFlag args
        pretty = "--pretty" `elem` flags
    input <- case files of
        [] -> TIO.getContents
        [f] -> TIO.readFile f
        _ -> do
            hPutStrLn stderr "Usage: organ-validate [--pretty] [file.json]"
            exitFailure
    case parseOrganIR input of
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
