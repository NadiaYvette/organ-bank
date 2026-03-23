-- | Zig ZIR Extraction Shim
--
-- Invokes the zir-extract Zig tool (built from zig-tool/) which uses
-- std.zig.AstGen to parse a Zig source file and produce ZIR.
-- The tool emits OrganIR JSON directly on stdout.
--
-- ZIR is *untyped* IR — it is the output of AstGen, before semantic
-- analysis (Sema). It captures the syntactic structure of the code
-- as Zig IR instructions but does not resolve types.

module OrganBank.ZigShim
  ( extractOrganIR
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

-- | Extract ZIR from a Zig source file and return OrganIR JSON.
--
-- Locates the zir-extract binary (built by zig build in zig-tool/),
-- invokes it on the given file, and returns the JSON output.
extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
  toolPath <- locateZirExtract
  case toolPath of
    Nothing -> pure $ Left
      "Cannot find zir-extract binary. Build it with: cd zig-tool && zig build"
    Just tool -> do
      (exitCode, stdout', stderr') <- readProcessWithExitCode tool [inputPath] ""
      case exitCode of
        ExitSuccess -> pure $ Right (T.pack stdout')
        ExitFailure _ -> pure $ Left $
          "zir-extract failed:\n" <> stderr'

-- | Locate the zir-extract binary.
--
-- Search order:
-- 1. ./zig-tool/zig-out/bin/zir-extract (relative to CWD)
-- 2. ../zig-tool/zig-out/bin/zir-extract (one level up, for running from src/)
-- 3. The directory containing this executable + /zig-tool/zig-out/bin/zir-extract
locateZirExtract :: IO (Maybe FilePath)
locateZirExtract = do
  cwd <- getCurrentDirectory
  let candidates =
        [ cwd </> "zig-tool" </> "zig-out" </> "bin" </> "zir-extract"
        , cwd </> ".." </> "zig-tool" </> "zig-out" </> "bin" </> "zir-extract"
        ]
  findFirst candidates

findFirst :: [FilePath] -> IO (Maybe FilePath)
findFirst [] = pure Nothing
findFirst (p:ps) = do
  exists <- doesFileExist p
  if exists then pure (Just p)
  else findFirst ps
