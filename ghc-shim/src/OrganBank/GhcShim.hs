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
import Data.Text qualified as T
import GHC
import GHC.Core (CoreBind, CoreProgram, Bind (..))
import GHC.Driver.Session (updOptLevel)
import GHC.Types.Name (getOccString)
import GHC.Types.Name qualified as GN
import GHC.Types.Unique (getKey)
import GHC.Types.Var (Var, varName)
import GHC.Unit.Module (moduleName, moduleNameString)
import GHC.Unit.Module.ModGuts (mg_binds)
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Process (readProcess)

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
        pure $ Right $ emitOrganIR modName inputPath coreProg

detectLibDir :: IO FilePath
detectLibDir = do
  raw <- readProcess "ghc" ["--print-libdir"] ""
  pure $ reverse $ dropWhile (== '\n') $ reverse raw

-- | Emit a CoreProgram as OrganIR JSON using the organ-ir library.
emitOrganIR :: String -> FilePath -> CoreProgram -> Text
emitOrganIR modName srcFile binds =
    renderOrganIR $
        IR.simpleOrganIR IR.LHaskell "ghc-shim-0.1" (T.pack modName) srcFile $
            concatMap bindToDefs binds

bindToDefs :: CoreBind -> [IR.Definition]
bindToDefs (NonRec v _e) = [varToDef v]
bindToDefs (Rec pairs) = [varToDef v | (v, _e) <- pairs]

varToDef :: Var -> IR.Definition
varToDef v =
    let name = T.pack (getOccString (varName v))
        uniq = getKey (GN.nameUnique (varName v))
    in IR.Definition
        { IR.defName = IR.QName "" (IR.Name name (fromIntegral uniq))
        , IR.defType = IR.TAny
        , IR.defExpr = IR.ELit (IR.LitInt 0) -- placeholder until full Core translation
        , IR.defSort = IR.SFun
        , IR.defVisibility = IR.Public
        , IR.defArity = 0
        }
