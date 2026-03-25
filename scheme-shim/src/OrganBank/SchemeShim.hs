-- | Extract Guile Tree-IL from Scheme source and emit OrganIR JSON.
module OrganBank.SchemeShim (extractOrganIR) where

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
    Nothing -> return $ Left "Could not locate extract.scm script"
    Just scriptPath -> do
      -- Try guile3.0 first, then guile
      (ec, out, err) <- readProcessWithExitCode "guile3.0"
        [scriptPath, inputPath] ""
      case ec of
        ExitSuccess -> return $ validateJson (T.pack out)
        ExitFailure _ -> do
          (ec2, out2, err2) <- readProcessWithExitCode "guile"
            [scriptPath, inputPath] ""
          case ec2 of
            ExitSuccess -> return $ validateJson (T.pack out2)
            ExitFailure _ -> return $ Left $ "guile failed: " ++ err ++ "\n" ++ err2

-- | Find extract.scm by searching relative to the executable.
findScript :: IO (Maybe FilePath)
findScript = do
    exePath <- getExecutablePath
    let exeDir = takeDirectory exePath
        candidates =
            [ exeDir </> "scripts" </> "extract.scm"
            , exeDir </> ".." </> "scripts" </> "extract.scm"
            , exeDir </> ".." </> ".." </> "scripts" </> "extract.scm"
            , exeDir </> ".." </> ".." </> ".." </> ".." </> "scripts" </> "extract.scm"
            , exeDir </> ".." </> ".." </> ".." </> ".." </> ".." </> "scripts" </> "extract.scm"
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
