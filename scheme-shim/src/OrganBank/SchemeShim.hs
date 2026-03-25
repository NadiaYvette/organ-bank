-- | Extract Guile Tree-IL from Scheme source and emit OrganIR JSON.
module OrganBank.SchemeShim (extractOrganIR) where

import Control.Exception (SomeException, catch)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Parse (parseOrganIR)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.Process (readProcessWithExitCode)

-- | Detect guile version by running @guile --version@.
detectCompilerVersion :: IO Text
detectCompilerVersion = do
    (ec, out, _) <- readProcessWithExitCode "guile" ["--version"] ""
    pure $ case ec of
        ExitSuccess | (l : _) <- lines out -> "scheme-shim-0.1 (" <> T.strip (T.pack l) <> ")"
        _ -> "scheme-shim-0.1"
  `catch` \(_ :: SomeException) -> pure "scheme-shim-0.1"

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
  shimVer <- detectCompilerVersion
  mScript <- findScript
  case mScript of
    Nothing -> return $ Left "Could not locate extract.scm script"
    Just scriptPath -> do
      -- Try guile3.0 first, then guile
      (ec, out, err) <- readProcessWithExitCode "guile3.0"
        [scriptPath, inputPath] ""
      case ec of
        ExitSuccess -> return $ validateJson shimVer (T.pack out)
        ExitFailure _ -> do
          (ec2, out2, err2) <- readProcessWithExitCode "guile"
            [scriptPath, inputPath] ""
          case ec2 of
            ExitSuccess -> return $ validateJson shimVer (T.pack out2)
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

-- | Validate JSON output by parsing it as OrganIR, and inject shimVersion.
validateJson :: Text -> Text -> Either String Text
validateJson shimVer json =
    let patched = T.replace "\"scheme-shim-0.1\"" ("\"" <> shimVer <> "\"") json
    in case parseOrganIR patched of
        Left err -> Left $ "JSON parse error: " ++ T.unpack err
        Right _ir -> Right patched
