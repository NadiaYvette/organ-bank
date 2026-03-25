-- | Extract SBCL disassembly from Common Lisp and emit OrganIR JSON.
module OrganBank.ClShim (extractOrganIR) where

import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Parse (parseOrganIR)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.Process (readProcessWithExitCode)

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
  mScript <- findScript
  case mScript of
    Nothing -> return $ Left "Could not locate extract.lisp script"
    Just scriptPath -> do
      (ec, out, err) <- readProcessWithExitCode "sbcl"
        [ "--non-interactive"
        , "--disable-debugger"
        , "--load", scriptPath
        , "--eval", "(extract-organ-ir " ++ show inputPath ++ ")"
        , "--eval", "(sb-ext:exit)"
        ] ""
      case ec of
        ExitFailure _ -> return $ Left $ "sbcl failed: " ++ err
        ExitSuccess ->
          -- SBCL prints some startup noise; find the JSON block
          let outText = T.pack out
              jsonStart = T.breakOn "{" outText
          in case jsonStart of
               (_, json) | not (T.null json) -> return $ validateJson json
               _ -> return $ Left "No JSON output from SBCL"

-- | Find extract.lisp by searching relative to the executable.
findScript :: IO (Maybe FilePath)
findScript = do
    exePath <- getExecutablePath
    let exeDir = takeDirectory exePath
        candidates =
            [ exeDir </> "scripts" </> "extract.lisp"
            , exeDir </> ".." </> "scripts" </> "extract.lisp"
            , exeDir </> ".." </> ".." </> "scripts" </> "extract.lisp"
            , exeDir </> ".." </> ".." </> ".." </> ".." </> "scripts" </> "extract.lisp"
            , exeDir </> ".." </> ".." </> ".." </> ".." </> ".." </> "scripts" </> "extract.lisp"
            ]
    findFirst candidates

findFirst :: [FilePath] -> IO (Maybe FilePath)
findFirst [] = pure Nothing
findFirst (p:ps) = do
    exists <- doesFileExist p
    if exists then pure (Just p) else findFirst ps

-- | Validate JSON output by parsing it as OrganIR.
validateJson :: Text -> Either String Text
validateJson json = case parseOrganIR json of
    Left err -> Left $ "JSON parse error: " ++ T.unpack err
    Right _ir -> Right json
