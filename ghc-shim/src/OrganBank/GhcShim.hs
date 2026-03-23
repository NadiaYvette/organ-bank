-- | GHC Core Extraction Shim
--
-- Uses the GHC API to compile a Haskell source file through desugaring,
-- then emits the resulting Core as OrganIR JSON.
--
-- Based on Frankenstein's GhcBridge/Driver.hs and GhcBridge/CoreTranslate.hs.

module OrganBank.GhcShim
  ( extractOrganIR
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcess)

import GHC
import GHC.Driver.Session (updOptLevel)
import GHC.Core (CoreProgram, CoreBind(..), CoreExpr, Bind(..))
import GHC.Core.Opt.Monad (CoreToDo(..))
import GHC.Types.Var (Var, varName, isId, isTyVar)
import GHC.Types.Name (getOccString, nameUnique)
import GHC.Types.Unique (getKey)
import GHC.Types.Literal
import GHC.Core.DataCon (dataConName)
import GHC.Core.TyCon (tyConName)
import GHC.Unit.Module (moduleName, moduleNameString)
import GHC.HsToCore.Monad (DsM)
import GHC.Types.Demand (isStrictDmd, isAbsDmd)

-- | Extract GHC Core from a Haskell source file and emit OrganIR JSON.
extractOrganIR :: FilePath -> IO (Either Text Text)
extractOrganIR inputPath = do
  libdir <- detectLibDir
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' = updOptLevel 1 dflags  -- O1 for demand analysis
    setSessionDynFlags dflags'
    target <- guessTarget inputPath Nothing Nothing
    setTargets [target]
    _sf <- load LoadAllTargets
    modGraph <- getModuleGraph
    let summaries = mgModSummaries modGraph
    case summaries of
      [] -> pure $ Left "No modules found"
      (ms:_) -> do
        parsed    <- parseModule ms
        typecked  <- typecheckModule parsed
        desugared <- desugarModule typecked
        let modGuts  = dm_core_module desugared
            coreProg = mg_binds modGuts
            modName  = moduleNameString (moduleName (ms_mod ms))
        pure $ Right $ emitOrganIR modName coreProg

detectLibDir :: IO FilePath
detectLibDir = do
  raw <- readProcess "ghc" ["--print-libdir"] ""
  pure $ reverse $ dropWhile (== '\n') $ reverse raw

-- | Emit a CoreProgram as OrganIR JSON.
-- This is a simplified emitter; a full implementation would traverse
-- all GHC Core constructors.
emitOrganIR :: String -> CoreProgram -> Text
emitOrganIR modName binds =
  T.unlines
    [ "{"
    , "  \"schema_version\": \"1.0.0\","
    , "  \"metadata\": {"
    , "    \"source_language\": \"haskell\","
    , "    \"compiler_version\": \"ghc-9.14.1\","
    , "    \"shim_version\": \"0.1.0\""
    , "  },"
    , "  \"module\": {"
    , "    \"name\": " <> jsonStr (T.pack modName) <> ","
    , "    \"definitions\": [" <> T.intercalate ",\n" (map emitBind binds) <> "],"
    , "    \"data_types\": [],"
    , "    \"effect_decls\": []"
    , "  }"
    , "}"
    ]

emitBind :: CoreBind -> Text
emitBind (NonRec v e) = emitDef v e
emitBind (Rec pairs) = T.intercalate ",\n" [emitDef v e | (v, e) <- pairs]

emitDef :: Var -> CoreExpr -> Text
emitDef v _e =
  let name = T.pack (getOccString (varName v))
      uniq = getKey (nameUnique (varName v))
  in T.unlines
    [ "      {"
    , "        \"name\": {\"module\": \"\", \"name\": {\"text\": " <> jsonStr name <> ", \"unique\": " <> T.pack (show uniq) <> "}},"
    , "        \"type\": {\"con\": {\"qname\": {\"module\": \"std\", \"name\": {\"text\": \"any\"}}}},"
    , "        \"expr\": {\"elit\": {\"int\": 0}},"
    , "        \"sort\": \"fun\","
    , "        \"visibility\": \"public\""
    , "      }"
    ]

jsonStr :: Text -> Text
jsonStr s = "\"" <> T.concatMap escapeChar s <> "\""
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar c    = T.singleton c
