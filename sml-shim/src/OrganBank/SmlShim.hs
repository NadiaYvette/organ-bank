{- | Extract type basis from SML via MLton and parse source for definitions.
MLton has no IR dump flag, so this shim combines:
1. Type signatures from `mlton -stop tc -show-basis`
2. Source-level function structure parsing
-}
module OrganBank.SmlShim (extractOrganIR) where

import Control.Exception (SomeException, catch)
import Data.List (unsnoc)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Directory (removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName)
import System.Process (readProcessWithExitCode)

-- | Detect MLton version by running @mlton@ (no args prints version to stdout).
detectCompilerVersion :: IO Text
detectCompilerVersion = do
    (_ec, out, _err) <- readProcessWithExitCode "mlton" [] ""
    pure $ case lines out of
        (l : _) | not (null l) -> "sml-shim-0.1 (" <> T.strip (T.pack l) <> ")"
        _ -> "sml-shim-0.1"
  `catch` \(_ :: SomeException) -> pure "sml-shim-0.1"

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
    shimVer <- detectCompilerVersion
    -- MLton needs an .mlb file for compilation
    let mlbPath = "/tmp/organ-bank-sml.mlb"
        mlbContent = "$(SML_LIB)/basis/basis.mlb\n" ++ inputPath ++ "\n"
    writeFile mlbPath mlbContent
    -- Get type basis
    (ec, basisOut, _basisErr) <- readProcessWithExitCode "mlton" ["-stop", "tc", "-show-basis", "/dev/stdout", mlbPath] ""
    removeFile mlbPath `catch` (\(_ :: SomeException) -> return ())
    -- Parse source file regardless of whether basis worked
    source <- TIO.readFile inputPath
    let basis = case ec of
            ExitSuccess -> T.pack basisOut
            _ -> ""
        types = parseBasis basis
        defs = parseSmlSource source types
        modName = takeBaseName inputPath
        json = emitOrganIR shimVer modName inputPath defs
    return $ Right json

data SmlDef = SmlDef
    { sdName :: Text
    , sdType :: Text -- from basis, or "any"
    , sdBody :: Text -- source text of the body
    , sdArity :: Int
    }
    deriving (Show)

-- Parse MLton -show-basis output: "val factorial: int -> int"
parseBasis :: Text -> [(Text, Text)]
parseBasis t = concatMap parseLine (T.lines t)
  where
    parseLine l
        | "val " `T.isPrefixOf` T.strip l =
            let rest = T.drop 4 (T.strip l)
                (nm, afterName) = T.breakOn ":" rest
                ty = T.strip (T.drop 1 afterName)
             in [(T.strip nm, ty)]
        | otherwise = []

-- Parse SML source for fun/val declarations
parseSmlSource :: Text -> [(Text, Text)] -> [SmlDef]
parseSmlSource source = parseFunDecls (T.lines source)

parseFunDecls :: [Text] -> [(Text, Text)] -> [SmlDef]
parseFunDecls [] _ = []
parseFunDecls (l : ls) types
    | "fun " `T.isPrefixOf` T.strip l =
        let (nm, arity, bodyLines, rest) = parseFunBinding l ls
            ty = lookupType nm types
            body = T.unlines (l : bodyLines)
         in SmlDef nm ty body arity : parseFunDecls rest types
    | "val " `T.isPrefixOf` T.strip l && "fn " `T.isInfixOf` l =
        let rest2 = T.drop 4 (T.strip l)
            (nm, _) = T.breakOn " " rest2
            ty = lookupType nm types
         in SmlDef (T.strip nm) ty l 1 : parseFunDecls ls types
    | otherwise = parseFunDecls ls types

parseFunBinding :: Text -> [Text] -> (Text, Int, [Text], [Text])
parseFunBinding firstLine rest =
    let stripped = T.strip firstLine
        afterFun = T.drop 4 stripped -- drop "fun "
        -- Handle optional rec
        afterRec = if "rec " `T.isPrefixOf` afterFun then T.drop 4 afterFun else afterFun
        nm = T.takeWhile (\c -> c /= ' ' && c /= '(') afterRec
        -- Count parameters (words between name and =)
        afterName = T.drop (T.length nm) afterRec
        beforeEq = T.takeWhile (/= '=') afterName
        params = filter (not . T.null) $ T.words (T.strip beforeEq)
        arity = length params
        -- Collect continuation lines (| clauses)
        (contLines, remaining) = collectContinuation rest
     in (T.strip nm, arity, contLines, remaining)

collectContinuation :: [Text] -> ([Text], [Text])
collectContinuation [] = ([], [])
collectContinuation (l : ls)
    | "  |" `T.isPrefixOf` l || "  | " `T.isPrefixOf` l =
        let (more, rest) = collectContinuation ls in (l : more, rest)
    | "and " `T.isPrefixOf` T.strip l =
        -- another clause of the same fun declaration
        let (more, rest) = collectContinuation ls in (l : more, rest)
    | otherwise = ([], l : ls)

lookupType :: Text -> [(Text, Text)] -> Text
lookupType nm types = fromMaybe "any" (lookup nm types)

-- | Emit OrganIR JSON for SML definitions using the organ-ir library.
emitOrganIR :: Text -> String -> FilePath -> [SmlDef] -> Text
emitOrganIR shimVer modName srcFile defs =
    renderOrganIR $
        IR.simpleOrganIR IR.LSml shimVer (T.pack modName) srcFile $
            map smlDefToIR defs

smlDefToIR :: SmlDef -> IR.Definition
smlDefToIR d =
    IR.Definition
        { IR.defName = IR.localName (sdName d)
        , IR.defType = smlTypeToIR (sdType d)
        , IR.defExpr = IR.EApp (IR.eVar "source") [IR.eString (sdBody d)]
        , IR.defSort = IR.SFun
        , IR.defVisibility = IR.Public
        , IR.defArity = sdArity d
        }

-- | Translate SML type string to OrganIR type.
smlTypeToIR :: Text -> IR.Ty
smlTypeToIR "any" = IR.TAny
smlTypeToIR t
    | " -> " `T.isInfixOf` t =
        let parts = T.splitOn " -> " t
         in case unsnoc parts of
                Just (params, result) ->
                    IR.TFn
                        (map (IR.FnArg Nothing . IR.tCon) params)
                        IR.pureEffect
                        (IR.tCon result)
                Nothing -> IR.TAny
    | otherwise = IR.tCon t
