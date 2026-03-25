-- | Integration test for OrganIR: parse, validate, pretty-print, and round-trip
-- hardcoded JSON strings representing realistic frontend output.
module Main (main) where

import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Parse (parseOrganIR)
import OrganIR.Pretty (ppOrganIR)
import OrganIR.Types qualified as IR
import OrganIR.Validate (Severity (..), Warning (..), validateOrganIR)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    putStrLn "=== OrganIR Integration Tests ==="
    results <- mapM (uncurry runTest) tests
    let passed = length (filter id results)
        total = length results
    putStrLn $ "\n" ++ show passed ++ "/" ++ show total ++ " integration tests passed"
    if all id results then exitSuccess else exitFailure

-- | Run a single integration test: parse JSON -> validate -> pretty-print -> round-trip.
runTest :: String -> Text -> IO Bool
runTest name jsonInput = do
    -- Step 1: Parse
    case parseOrganIR jsonInput of
        Left err -> do
            putStrLn $ "  FAIL " ++ name ++ " [parse]: " ++ T.unpack err
            pure False
        Right ir -> do
            -- Step 2: Validate (no errors)
            let warnings = validateOrganIR ir
                errors = [w | w <- warnings, wSeverity w == Error]
            if not (null errors)
                then do
                    putStrLn $ "  FAIL " ++ name ++ " [validate]: errors found"
                    mapM_ (\w -> putStrLn $ "    " ++ T.unpack (wPath w) ++ ": " ++ T.unpack (wMessage w)) errors
                    pure False
                else do
                    -- Step 3: Pretty-print (non-empty output)
                    let pretty = ppOrganIR ir
                    if T.null pretty
                        then do
                            putStrLn $ "  FAIL " ++ name ++ " [pretty]: empty output"
                            pure False
                        else do
                            -- Step 4: Round-trip: render -> parse -> render, check stability
                            let rendered1 = renderOrganIR ir
                            case parseOrganIR rendered1 of
                                Left err2 -> do
                                    putStrLn $ "  FAIL " ++ name ++ " [roundtrip-parse]: " ++ T.unpack err2
                                    pure False
                                Right ir2 -> do
                                    let rendered2 = renderOrganIR ir2
                                    if rendered1 /= rendered2
                                        then do
                                            putStrLn $ "  FAIL " ++ name ++ " [roundtrip-stable]: render mismatch"
                                            putStrLn $ "    First:  " ++ take 200 (T.unpack rendered1)
                                            putStrLn $ "    Second: " ++ take 200 (T.unpack rendered2)
                                            pure False
                                        else do
                                            -- Step 5: Validate the round-tripped IR too
                                            let ws2 = validateOrganIR ir2
                                                errs2 = [w | w <- ws2, wSeverity w == Error]
                                            if not (null errs2)
                                                then do
                                                    putStrLn $ "  FAIL " ++ name ++ " [roundtrip-validate]: errors"
                                                    pure False
                                                else do
                                                    putStrLn $ "  PASS " ++ name
                                                    pure True

-- ---------------------------------------------------------------------------
-- Test cases: a mix of programmatically-built and raw JSON strings
-- ---------------------------------------------------------------------------

tests :: [(String, Text)]
tests =
    [ ("sml-style", renderOrganIR smlStyleIR)
    , ("erlang-style", renderOrganIR erlangStyleIR)
    , ("prolog-style", renderOrganIR prologStyleIR)
    , ("scheme-style", renderOrganIR schemeStyleIR)
    , ("lua-style", renderOrganIR luaStyleIR)
    , ("forth-style", renderOrganIR forthStyleIR)
    , ("programmatic-minimal", renderOrganIR minimalIR)
    , ("programmatic-full", renderOrganIR fullIR)
    , ("programmatic-effects", renderOrganIR effectsIR)
    ]

-- | Minimal valid IR built programmatically.
minimalIR :: IR.OrganIR
minimalIR =
    IR.simpleOrganIR IR.LHaskell "test-0.1" "Minimal" "minimal.hs"
        [IR.valDef "x" IR.TAny (IR.ELit (IR.LitInt 42))]

-- | Full IR with datatypes, effects, exports.
fullIR :: IR.OrganIR
fullIR = IR.OrganIR
    { IR.irMetadata = IR.Metadata IR.LHaskell (Just "ghc-9.14.1") (Just "Full.hs") "test-0.1" Nothing
    , IR.irModule = IR.Module "Full" ["main", "helper"] []
        [ IR.funDef "main"
            (IR.TFn [IR.FnArg Nothing IR.TAny] (IR.EffectRow [IR.localName "IO"] Nothing) IR.TAny)
            (IR.ELam [IR.LamParam (IR.Name "args" 0) Nothing]
                (IR.EApp (IR.EVar (IR.Name "println" 0)) [IR.ELit (IR.LitString "hello")]))
            1
        , IR.valDef "helper" (IR.TCon (IR.localName "Int")) (IR.ELit (IR.LitInt 99))
        ]
        [ IR.DataType (IR.localName "Maybe")
            [IR.TyVar (IR.Name "a" 0) Nothing]
            [ IR.Constructor (IR.localName "Nothing") []
            , IR.Constructor (IR.localName "Just") [IR.TVar (IR.Name "a" 0)]
            ]
        ]
        []
    }

-- | IR with algebraic effects.
effectsIR :: IR.OrganIR
effectsIR = IR.OrganIR
    { IR.irMetadata = IR.Metadata IR.LKoka Nothing Nothing "test-0.1" Nothing
    , IR.irModule = IR.Module "Effects" [] []
        [ IR.valDef "x" IR.TAny
            (IR.EHandle (IR.localName "State")
                (IR.EPerform (IR.localName "State") "get" [])
                (IR.ELam [IR.LamParam (IR.Name "s" 0) Nothing] (IR.EVar (IR.Name "s" 0))))
        ]
        []
        [ IR.EffectDecl (IR.localName "State")
            [IR.TyVar (IR.Name "s" 0) Nothing]
            [ IR.Operation "get"
                (IR.TFn [] (IR.EffectRow [IR.localName "State"] Nothing) (IR.TVar (IR.Name "s" 0)))
            , IR.Operation "put"
                (IR.TFn [IR.FnArg Nothing (IR.TVar (IR.Name "s" 0))] (IR.EffectRow [IR.localName "State"] Nothing) IR.TAny)
            ]
        ]
    }

-- ---------------------------------------------------------------------------
-- Frontend-style IR, built programmatically (mirrors real frontend output)
-- ---------------------------------------------------------------------------

-- | Helper: build a simple "hello" IR for a given language.
helloIR :: IR.SourceLang -> Text -> Text -> Text -> IR.OrganIR
helloIR lang shimVer fnName msg =
    IR.OrganIR
        { IR.irMetadata = IR.Metadata lang Nothing Nothing shimVer Nothing
        , IR.irModule = IR.Module "hello" [] []
            [ IR.valDef "main" IR.TAny
                (IR.EApp (IR.EVar (IR.Name fnName 0)) [IR.ELit (IR.LitString msg)])
            ]
            [] []
        }

smlStyleIR :: IR.OrganIR
smlStyleIR = helloIR IR.LSml "sml-organ-fe-0.1" "print" "Hello from SML"

erlangStyleIR :: IR.OrganIR
erlangStyleIR =
    let base = helloIR IR.LErlang "erlang-organ-fe-0.1" "io:format" "Hello from Erlang~n"
    in base { IR.irModule = (IR.irModule base)
                { IR.modExports = ["main"]
                , IR.modDefs =
                    [ IR.funDef "main" IR.TAny
                        (IR.EApp (IR.EVar (IR.Name "io:format" 0)) [IR.ELit (IR.LitString "Hello from Erlang~n")])
                        1
                    ]
                }
            }

prologStyleIR :: IR.OrganIR
prologStyleIR = helloIR IR.LProlog "prolog-organ-fe-0.1" "write" "Hello from Prolog"

schemeStyleIR :: IR.OrganIR
schemeStyleIR = helloIR IR.LScheme "scheme-organ-fe-0.1" "display" "Hello from Scheme"

luaStyleIR :: IR.OrganIR
luaStyleIR = helloIR IR.LLua "lua-organ-fe-0.1" "print" "Hello from Lua"

forthStyleIR :: IR.OrganIR
forthStyleIR = helloIR IR.LForth "forth-organ-fe-0.1" ".\"" "Hello from Forth"
