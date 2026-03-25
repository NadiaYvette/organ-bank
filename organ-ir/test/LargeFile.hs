-- | Performance test: generate an OrganIR with 1000 definitions and
-- verify that parse, render, and validate all complete in reasonable
-- time (under 5 seconds).
module Main (main) where

import Data.Text qualified as T
import OrganIR.Json (renderOrganIR)
import OrganIR.Parse (parseOrganIR)
import OrganIR.Parse qualified
import OrganIR.Schema (schemaCheck)
import OrganIR.Types
import OrganIR.Validate (validateOrganIR)
import System.CPUTime (getCPUTime)
import System.Exit (exitFailure, exitSuccess)

-- | Generate a simple definition with a unique name.
mkDef :: Int -> Definition
mkDef i = Definition
    { defName = QName "LargeModule" (Name ("def_" <> T.pack (show i)) i)
    , defType = TFn [FnArg Nothing TAny] (EffectRow [] Nothing) TAny
    , defExpr = ELam [LamParam (Name "x" 0) Nothing]
                  (EApp (EVar (Name "id" 0)) [EVar (Name "x" 0)])
    , defSort = SFun
    , defVisibility = Public
    , defArity = 1
    }

-- | Generate an OrganIR document with N definitions.
mkLargeIR :: Int -> OrganIR
mkLargeIR n = OrganIR
    { irMetadata = Metadata
        { metaSourceLang = LHaskell
        , metaCompilerVersion = Just "9.14.1"
        , metaSourceFile = Just "LargeFile.hs"
        , metaShimVersion = "0.1.0"
        , metaTimestamp = Nothing
        }
    , irModule = Module
        { modName = "LargeModule"
        , modExports = ["def_0", "def_1"]
        , modImports = []
        , modDefs = map mkDef [0 .. n - 1]
        , modDataTypes = []
        , modEffectDecls = []
        }
    }

-- | Time an IO action, returning elapsed CPU seconds and the result.
timed :: IO a -> IO (Double, a)
timed action = do
    t0 <- getCPUTime
    result <- action
    t1 <- getCPUTime
    let secs = fromIntegral (t1 - t0) / (1e12 :: Double)
    pure (secs, result)

main :: IO ()
main = do
    let n = 1000
    putStrLn $ "Generating OrganIR with " <> show n <> " definitions..."

    let ir = mkLargeIR n
    -- Force the structure
    putStrLn $ "Module has " <> show (length (modDefs (irModule ir))) <> " definitions."

    -- Render
    putStrLn "Rendering to JSON..."
    (renderTime, json) <- timed $ do
        let j = renderOrganIR ir
        -- Force the text by checking its length
        pure $! T.length j `seq` j
    putStrLn $ "  Render: " <> show renderTime <> "s (" <> show (T.length json) <> " chars)"

    -- Parse
    putStrLn "Parsing JSON..."
    (parseTime, parseResult) <- timed $ do
        let r = parseOrganIR json
        case r of
            Left err -> pure (Left err)
            Right ir' -> length (modDefs (irModule ir')) `seq` pure (Right ir')
    case parseResult of
        Left err -> do
            putStrLn $ "  Parse FAILED: " <> T.unpack err
            exitFailure
        Right ir' ->
            putStrLn $ "  Parse: " <> show parseTime <> "s (" <> show (length (modDefs (irModule ir'))) <> " defs)"

    -- Schema check
    putStrLn "Schema checking..."
    (schemaTime, schemaResult) <- timed $ do
        case parseResult of
            Left _ -> pure []
            Right _ ->
                -- Re-parse to get JVal for schema check
                case OrganIR.Parse.parseJSON json of
                    Left _ -> pure []
                    Right jv -> let errs = schemaCheck jv in length errs `seq` pure errs
    putStrLn $ "  Schema: " <> show schemaTime <> "s (" <> show (length schemaResult) <> " errors)"

    -- Validate
    putStrLn "Validating..."
    (validateTime, warnings) <- timed $ case parseResult of
        Left _ -> pure []
        Right ir' -> let ws = validateOrganIR ir' in length ws `seq` pure ws
    putStrLn $ "  Validate: " <> show validateTime <> "s (" <> show (length warnings) <> " warnings)"

    -- Round-trip check
    putStrLn "Round-trip check..."
    case parseResult of
        Left _ -> putStrLn "  Skipped (parse failed)"
        Right ir' -> do
            let json2 = renderOrganIR ir'
            if json == json2
                then putStrLn "  Round-trip: OK"
                else do
                    putStrLn "  Round-trip: MISMATCH"
                    exitFailure

    -- Time budget check
    let totalTime = renderTime + parseTime + schemaTime + validateTime
    putStrLn $ "\nTotal CPU time: " <> show totalTime <> "s"
    if totalTime < 5.0
        then putStrLn "PASS: under 5-second budget." >> exitSuccess
        else putStrLn "FAIL: exceeded 5-second budget." >> exitFailure

