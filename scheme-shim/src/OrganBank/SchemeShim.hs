-- | Extract Guile Tree-IL from Scheme source and emit OrganIR JSON.
module OrganBank.SchemeShim (extractOrganIR) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
  let scriptPath = "/home/nyc/src/organ-bank/scheme-shim/scripts/extract.scm"
  -- Try guile3.0 first, then guile
  (ec, out, err) <- readProcessWithExitCode "guile3.0"
    [scriptPath, inputPath] ""
  case ec of
    ExitSuccess -> return $ Right (T.pack out)
    ExitFailure _ -> do
      (ec2, out2, err2) <- readProcessWithExitCode "guile"
        [scriptPath, inputPath] ""
      case ec2 of
        ExitSuccess -> return $ Right (T.pack out2)
        ExitFailure _ -> return $ Left $ "guile failed: " ++ err ++ "\n" ++ err2
