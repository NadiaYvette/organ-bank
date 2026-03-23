-- | Extract SBCL disassembly from Common Lisp and emit OrganIR JSON.
module OrganBank.ClShim (extractOrganIR) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory)

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
  -- Find the extract.lisp script relative to the shim
  let scriptPath = findScript
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
           (_, json) | not (T.null json) -> return $ Right json
           _ -> return $ Left "No JSON output from SBCL"

-- The script is installed alongside the shim in scripts/
findScript :: FilePath
findScript = "/home/nyc/src/organ-bank/cl-shim/scripts/extract.lisp"
