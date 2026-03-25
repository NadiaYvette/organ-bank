-- | Julia Typed IR Extraction Shim
--
-- Invokes a Julia script (extract.jl) that uses code_typed() to extract
-- Julia's typed SSA IR, then passes the resulting OrganIR JSON through.

module OrganBank.JuliaShim
  ( extractOrganIR
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Parse (parseOrganIR)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.Process (readProcessWithExitCode)

-- | Extract Julia typed IR from a .jl source file and return OrganIR JSON.
--
-- Locates extract.jl relative to the executable, then invokes:
--   julia extract.jl <file>
--
-- The Julia script does all the real work: include()ing the target file,
-- discovering functions, calling code_typed(), and emitting JSON.
extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
  scriptPath <- findExtractScript
  case scriptPath of
    Nothing -> pure $ Left "Could not locate extract.jl script"
    Just script -> do
      (exitCode, stdout, stderr_) <-
        readProcessWithExitCode "julia" ["--startup-file=no", script, inputPath] ""
      case exitCode of
        ExitSuccess -> pure $ validateJson (T.pack stdout)
        ExitFailure code ->
          pure $ Left $
            "julia exited with code " <> show code <> ":\n" <> stderr_

-- | Find extract.jl by searching relative to the executable,
-- then falling back to common locations.
findExtractScript :: IO (Maybe FilePath)
findExtractScript = do
  exePath <- getExecutablePath
  let exeDir = takeDirectory exePath
      candidates =
        [ exeDir </> "scripts" </> "extract.jl"
        , exeDir </> ".." </> "scripts" </> "extract.jl"
        , exeDir </> ".." </> ".." </> "scripts" </> "extract.jl"
        -- During development, look relative to the source tree
        , exeDir </> ".." </> ".." </> ".." </> ".." </> "scripts" </> "extract.jl"
        , exeDir </> ".." </> ".." </> ".." </> ".." </> ".." </> "scripts" </> "extract.jl"
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
