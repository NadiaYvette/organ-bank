{- | Mercury HLDS Extraction Shim

Runs mmc --dump-hlds 50 to get the HLDS dump after mode/determinism
analysis, then parses the text output and emits OrganIR JSON.

Based on Frankenstein's MercuryBridge/HldsParse.hs.
-}
module OrganBank.MmcShim (
    extractOrganIR,
) where

import Control.Exception qualified
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

-- | Extract Mercury HLDS from a source file and emit OrganIR JSON.
extractOrganIR :: FilePath -> IO (Either Text Text)
extractOrganIR inputPath = do
    -- Mercury compiler dumps HLDS to stderr with --dump-hlds
    (ec, _stdout, stderrOut) <-
        readProcessWithExitCode "mmc" ["--dump-hlds", "50", inputPath] ""
    -- mmc may return non-zero even on success with --dump-hlds
    let hldsText = stderrOut
    -- Also check for .hlds file
    let hldsFile = takeBaseName inputPath ++ ".hlds_dump"
    hldsFromFile <- tryReadFile hldsFile
    let dump = fromMaybe hldsText hldsFromFile
    case ec of
        ExitSuccess -> pure ()
        ExitFailure _ ->
            hPutStrLn stderr "mmc exited with error (dump may still be usable)"
    if null dump
        then pure $ Left "No HLDS output from mmc"
        else do
            let modName = takeBaseName inputPath
                preds = parseHldsDump dump
                defs = zipWith predToIR [1 ..] preds
                exports = map (T.pack . predName) preds
                ir =
                    renderOrganIR $
                        IR.OrganIR
                            { IR.irMetadata = IR.Metadata IR.LMercury Nothing Nothing "mmc-shim-0.1" Nothing
                            , IR.irModule = IR.Module (T.pack modName) exports defs [] []
                            }
            pure $ Right ir

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile fp = do
    result <- Control.Exception.try @IOError (readFile fp >>= \c -> length c `seq` pure c)
    pure $ either (const Nothing) Just result

-- | A parsed Mercury predicate from HLDS dump.
data HldsPred = HldsPred
    { predName :: String
    , predArity :: Int
    , predDet :: String -- "det", "semidet", "multi", "nondet", "erroneous", "failure"
    , predModes :: [String] -- mode declarations
    }
    deriving (Show)

-- | Parse the HLDS dump text to extract predicate declarations.
parseHldsDump :: String -> [HldsPred]
parseHldsDump dump =
    let ls = lines dump
     in extractPreds ls

extractPreds :: [String] -> [HldsPred]
extractPreds [] = []
extractPreds (l : ls)
    | ":- pred " `isPrefixOf` stripped =
        let predDecl = drop 8 stripped -- drop ":- pred "
            (name', rest) = break (== '(') predDecl
            arity = countArgs rest
            (det, modes, remaining) = findDetAndModes ls
         in HldsPred (trim name') arity det modes : extractPreds remaining
    | otherwise = extractPreds ls
  where
    stripped = dropWhile isSpace l

countArgs :: String -> Int
countArgs s =
    let inner = takeWhile (/= ')') (drop 1 s) -- drop the '('
     in if null inner
            then 0
            else length (filter (== ',') inner) + 1

findDetAndModes :: [String] -> (String, [String], [String])
findDetAndModes ls =
    let (block, rest) = span (\l' -> not (":- pred " `isPrefixOf` dropWhile isSpace l') && not (null l')) ls
        det = findDet block
        modes = findModes block
     in (det, modes, rest)

findDet :: [String] -> String
findDet [] = "det"
findDet (l : ls)
    | "is det" `isInfixOf'` l = "det"
    | "is semidet" `isInfixOf'` l = "semidet"
    | "is multi" `isInfixOf'` l = "multi"
    | "is nondet" `isInfixOf'` l = "nondet"
    | "is erroneous" `isInfixOf'` l = "erroneous"
    | "is failure" `isInfixOf'` l = "failure"
    | otherwise = findDet ls

isInfixOf' :: String -> String -> Bool
isInfixOf' needle haystack = any (isPrefixOf needle) (tails' haystack)
  where
    tails' [] = [[]]
    tails' s@(_ : xs) = s : tails' xs

findModes :: [String] -> [String]
findModes ls =
    [trim l | l <- ls, ":- mode " `isPrefixOf` dropWhile isSpace l]

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Convert Mercury determinism to an OrganIR effect row.
detToEffectRow :: String -> IR.EffectRow
detToEffectRow "det" = IR.pureEffect
detToEffectRow "semidet" = IR.EffectRow [IR.qualName "std" "exn"] Nothing
detToEffectRow "multi" = IR.EffectRow [IR.qualName "std" "choice"] Nothing
detToEffectRow "nondet" = IR.EffectRow [IR.qualName "std" "exn", IR.qualName "std" "choice"] Nothing
detToEffectRow "erroneous" = IR.EffectRow [IR.qualName "std" "exn"] Nothing
detToEffectRow "failure" = IR.EffectRow [IR.qualName "std" "exn"] Nothing
detToEffectRow _ = IR.pureEffect

-- | Convert a parsed HLDS predicate to an OrganIR definition.
predToIR :: Int -> HldsPred -> IR.Definition
predToIR uid pred' =
    IR.Definition
        { IR.defName = IR.QName "" (IR.Name (T.pack (predName pred')) uid)
        , IR.defType =
            IR.TFn
                (replicate (predArity pred') (IR.FnArg (Just IR.Many) IR.TAny))
                (detToEffectRow (predDet pred'))
                IR.TAny
        , IR.defExpr = IR.ELit (IR.LitInt 0)
        , IR.defSort = IR.SFun
        , IR.defVisibility = IR.Public
        , IR.defArity = predArity pred'
        }
