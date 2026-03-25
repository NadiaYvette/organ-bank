-- | Round-trip test for OrganIR binary serialization.
-- Tests encode -> decode -> re-encode, checking byte-level stability.
-- Also cross-checks against JSON: binary decode must produce the same JSON as direct render.
module Main (main) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Binary (decodeOrganIR, encodeOrganIR)
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    putStrLn "=== Binary round-trip: Expression tests ==="
    exprResults <- mapM (uncurry testExpr) exprTests

    putStrLn "\n=== Binary round-trip: Type tests ==="
    typeResults <- mapM (uncurry testType) typeTests

    putStrLn "\n=== Binary round-trip: SourceLang tests ==="
    langResults <- mapM (uncurry testLang) langTests

    putStrLn "\n=== Binary round-trip: Structure tests ==="
    structResults <- mapM (uncurry testIR) structTests

    putStrLn "\n=== Binary format validation tests ==="
    formatResults <- formatTests

    let allResults = exprResults ++ typeResults ++ langResults ++ structResults ++ formatResults
        passed = length (filter id allResults)
        total = length allResults
    putStrLn $ "\n" ++ show passed ++ "/" ++ show total ++ " tests passed"
    if all id allResults then exitSuccess else exitFailure

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

wrapExpr :: String -> IR.Expr -> IR.OrganIR
wrapExpr nm expr =
    IR.simpleOrganIR IR.LHaskell "test-0.1" "Test" "test.hs"
        [IR.Definition (IR.localName (T.pack nm)) IR.TAny expr IR.SVal IR.Public 0]

wrapType :: String -> IR.Ty -> IR.OrganIR
wrapType nm ty =
    IR.simpleOrganIR IR.LHaskell "test-0.1" "Test" "test.hs"
        [IR.Definition (IR.localName (T.pack nm)) ty (IR.ELit (IR.LitInt 0)) IR.SVal IR.Public 0]

-- | Binary round-trip: encode -> decode -> re-encode, check bytes match.
-- Also cross-check: JSON of decoded value must match JSON of original.
binaryRoundTrip :: String -> IR.OrganIR -> IO Bool
binaryRoundTrip nm ir = do
    let bytes1 = encodeOrganIR ir
    case decodeOrganIR bytes1 of
        Left err -> do
            putStrLn $ "  FAIL " ++ nm ++ ": decode failed: " ++ err
            return False
        Right ir2 -> do
            let bytes2 = encodeOrganIR ir2
            if bytes1 /= bytes2
                then do
                    putStrLn $ "  FAIL " ++ nm ++ ": binary mismatch after round-trip"
                    putStrLn $ "    First:  " ++ show (BS.length bytes1) ++ " bytes"
                    putStrLn $ "    Second: " ++ show (BS.length bytes2) ++ " bytes"
                    return False
                else do
                    -- Cross-check: JSON must match
                    let json1 = renderOrganIR ir
                        json2 = renderOrganIR ir2
                    if json1 /= json2
                        then do
                            putStrLn $ "  FAIL " ++ nm ++ ": JSON mismatch after binary round-trip"
                            putStrLn $ "    Original JSON: " ++ take 200 (T.unpack json1)
                            putStrLn $ "    Decoded JSON:  " ++ take 200 (T.unpack json2)
                            return False
                        else do
                            putStrLn $ "  PASS " ++ nm ++ " (" ++ show (BS.length bytes1) ++ " bytes)"
                            return True

testExpr :: String -> IR.Expr -> IO Bool
testExpr nm expr = binaryRoundTrip nm (wrapExpr nm expr)

testType :: String -> IR.Ty -> IO Bool
testType nm ty = binaryRoundTrip nm (wrapType nm ty)

testLang :: String -> IR.SourceLang -> IO Bool
testLang nm lang = do
    let ir = IR.OrganIR
                { IR.irMetadata = IR.Metadata lang Nothing Nothing "test-0.1" Nothing
                , IR.irModule = IR.Module "Test" [] [] [IR.valDef "x" IR.TAny (IR.ELit (IR.LitInt 1))] [] []
                }
    binaryRoundTrip nm ir

testIR :: String -> IR.OrganIR -> IO Bool
testIR = binaryRoundTrip

-- --------------------------------------------------------------------------
-- Format validation tests
-- --------------------------------------------------------------------------

formatTests :: IO [Bool]
formatTests = sequence
    [ testBadMagic
    , testBadVersion
    , testTruncated
    , testTrailingBytes
    ]

testBadMagic :: IO Bool
testBadMagic = do
    let bs = BS.pack [0x00, 0x00, 0x00, 0x00, 0, 1]
    case decodeOrganIR bs of
        Left err | "bad magic" `isInfixOfStr` err -> do
            putStrLn "  PASS bad-magic (rejected with clear error)"
            return True
        Left err -> do
            putStrLn $ "  FAIL bad-magic: wrong error: " ++ err
            return False
        Right _ -> do
            putStrLn "  FAIL bad-magic: should have been rejected"
            return False

testBadVersion :: IO Bool
testBadVersion = do
    let bs = BS.pack [0x4F, 0x52, 0x49, 0x52, 1, 0]
    case decodeOrganIR bs of
        Left err | "unsupported version" `isInfixOfStr` err -> do
            putStrLn "  PASS bad-version (rejected with clear error)"
            return True
        Left err -> do
            putStrLn $ "  FAIL bad-version: wrong error: " ++ err
            return False
        Right _ -> do
            putStrLn "  FAIL bad-version: should have been rejected"
            return False

testTruncated :: IO Bool
testTruncated = do
    let bs = BS.pack [0x4F, 0x52, 0x49, 0x52, 0, 1]
    case decodeOrganIR bs of
        Left _ -> do
            putStrLn "  PASS truncated (rejected)"
            return True
        Right _ -> do
            putStrLn "  FAIL truncated: should have been rejected"
            return False

testTrailingBytes :: IO Bool
testTrailingBytes = do
    let ir = IR.organIR IR.LHaskell "t" "T" [IR.valDef "x" IR.TAny (IR.ELit (IR.LitInt 0))]
        bs = encodeOrganIR ir <> BS.singleton 0xFF
    case decodeOrganIR bs of
        Left err | "trailing" `isInfixOfStr` err -> do
            putStrLn "  PASS trailing-bytes (rejected with clear error)"
            return True
        Left err -> do
            putStrLn $ "  FAIL trailing-bytes: wrong error: " ++ err
            return False
        Right _ -> do
            putStrLn "  FAIL trailing-bytes: should have been rejected"
            return False

isInfixOfStr :: String -> String -> Bool
isInfixOfStr needle haystack = any (isPrefixOfStr needle) (tails' haystack)
  where
    isPrefixOfStr [] _ = True
    isPrefixOfStr _ [] = False
    isPrefixOfStr (a:as) (b:bs') = a == b && isPrefixOfStr as bs'
    tails' [] = [[]]
    tails' s@(_:xs) = s : tails' xs

-- --------------------------------------------------------------------------
-- Expression tests (same fixtures as RoundTrip.hs)
-- --------------------------------------------------------------------------

n :: Text -> IR.Name
n t = IR.Name t 0

qn :: Text -> IR.QName
qn t = IR.QName "" (n t)

qnMod :: Text -> Text -> IR.QName
qnMod m t = IR.QName m (n t)

exprTests :: [(String, IR.Expr)]
exprTests =
    [ ("EVar", IR.EVar (n "x"))
    , ("ELit-Int", IR.ELit (IR.LitInt 42))
    , ("ELit-Int-negative", IR.ELit (IR.LitInt (-99)))
    , ("ELit-Int-zero", IR.ELit (IR.LitInt 0))
    , ("ELit-Float", IR.ELit (IR.LitFloat 3.14))
    , ("ELit-Float-negative", IR.ELit (IR.LitFloat (-2.5)))
    , ("ELit-String", IR.ELit (IR.LitString "hello"))
    , ("ELit-String-empty", IR.ELit (IR.LitString ""))
    , ("ELit-String-escapes", IR.ELit (IR.LitString "line1\nline2\ttab\\slash\"quote"))
    , ("ELit-Bool-True", IR.ELit (IR.LitBool True))
    , ("ELit-Bool-False", IR.ELit (IR.LitBool False))
    , ("ECon-nullary", IR.ECon (qn "True") [])
    , ("ECon-unary", IR.ECon (qn "Just") [IR.ELit (IR.LitInt 1)])
    , ("ECon-binary", IR.ECon (qn "Pair") [IR.ELit (IR.LitInt 1), IR.ELit (IR.LitString "two")])
    , ("ECon-qualified", IR.ECon (qnMod "Data.Maybe" "Just") [IR.EVar (n "x")])
    , ("EApp-single", IR.EApp (IR.EVar (n "f")) [IR.EVar (n "x")])
    , ("EApp-multi", IR.EApp (IR.EVar (n "f")) [IR.EVar (n "x"), IR.EVar (n "y"), IR.EVar (n "z")])
    , ("ELam-untyped", IR.ELam [IR.LamParam (n "x") Nothing] (IR.EVar (n "x")))
    , ("ELam-typed", IR.ELam [IR.LamParam (n "x") (Just IR.TAny)] (IR.EVar (n "x")))
    , ( "ELam-multi-params"
      , IR.ELam
            [ IR.LamParam (n "x") Nothing
            , IR.LamParam (n "y") (Just (IR.TCon (qn "Int")))
            ]
            (IR.EApp (IR.EVar (n "add")) [IR.EVar (n "x"), IR.EVar (n "y")])
      )
    , ( "ELet-single"
      , IR.ELet
            [IR.LetBind (n "x") Nothing (IR.ELit (IR.LitInt 1))]
            (IR.EVar (n "x"))
      )
    , ( "ELet-typed"
      , IR.ELet
            [IR.LetBind (n "x") (Just IR.TAny) (IR.ELit (IR.LitInt 1))]
            (IR.EVar (n "x"))
      )
    , ( "ELet-multi"
      , IR.ELet
            [ IR.LetBind (n "x") Nothing (IR.ELit (IR.LitInt 1))
            , IR.LetBind (n "y") Nothing (IR.ELit (IR.LitInt 2))
            ]
            (IR.EApp (IR.EVar (n "add")) [IR.EVar (n "x"), IR.EVar (n "y")])
      )
    , ( "ECase-PatCon"
      , IR.ECase (IR.EVar (n "x"))
            [ IR.Branch (IR.PatCon (qn "Just") [IR.PatBinder (n "v") Nothing]) (IR.EVar (n "v"))
            , IR.Branch (IR.PatCon (qn "Nothing") []) (IR.ELit (IR.LitInt 0))
            ]
      )
    , ( "ECase-PatCon-typed-binder"
      , IR.ECase (IR.EVar (n "x"))
            [ IR.Branch
                (IR.PatCon (qn "Just") [IR.PatBinder (n "v") (Just IR.TAny)])
                (IR.EVar (n "v"))
            ]
      )
    , ( "ECase-PatLit"
      , IR.ECase (IR.EVar (n "x"))
            [ IR.Branch (IR.PatLit (IR.LitInt 0)) (IR.ELit (IR.LitString "zero"))
            , IR.Branch (IR.PatLit (IR.LitInt 1)) (IR.ELit (IR.LitString "one"))
            ]
      )
    , ( "ECase-PatVar"
      , IR.ECase (IR.EVar (n "x"))
            [ IR.Branch (IR.PatVar (n "y") Nothing) (IR.EVar (n "y")) ]
      )
    , ( "ECase-PatVar-typed"
      , IR.ECase (IR.EVar (n "x"))
            [ IR.Branch (IR.PatVar (n "y") (Just (IR.TCon (qn "Int")))) (IR.EVar (n "y")) ]
      )
    , ( "ECase-PatWild"
      , IR.ECase (IR.EVar (n "x"))
            [ IR.Branch IR.PatWild (IR.ELit (IR.LitInt 0)) ]
      )
    , ("ETypeApp", IR.ETypeApp (IR.EVar (n "id")) [IR.TCon (qn "Int")])
    , ("ETypeApp-multi", IR.ETypeApp (IR.EVar (n "pair")) [IR.TCon (qn "Int"), IR.TCon (qn "String")])
    , ("ETypeLam", IR.ETypeLam [IR.TyVar (n "a") Nothing] (IR.EVar (n "x")))
    , ("ETypeLam-with-kind", IR.ETypeLam [IR.TyVar (n "a") (Just "Type")] (IR.EVar (n "x")))
    , ( "ETypeLam-multi"
      , IR.ETypeLam
            [ IR.TyVar (n "a") Nothing
            , IR.TyVar (n "b") (Just "Type -> Type")
            ]
            (IR.EVar (n "x"))
      )
    , ("EPerform-no-args", IR.EPerform (qn "State") "get" [])
    , ("EPerform-with-args", IR.EPerform (qn "State") "put" [IR.ELit (IR.LitInt 42)])
    , ("EPerform-multi-args", IR.EPerform (qn "IO") "write" [IR.ELit (IR.LitString "hello"), IR.ELit (IR.LitInt 1)])
    , ("EHandle", IR.EHandle (qn "State") (IR.EVar (n "body")) (IR.EVar (n "handler")))
    , ("ERetain", IR.ERetain (n "x"))
    , ("ERelease", IR.ERelease (n "x"))
    , ("EDrop", IR.EDrop (n "x"))
    , ("EReuse", IR.EReuse (n "x"))
    , ("EDelay", IR.EDelay (IR.ELit (IR.LitInt 42)))
    , ("EForce", IR.EForce (IR.EVar (n "thunk")))
    , ("ETuple-empty", IR.ETuple [])
    , ("ETuple-pair", IR.ETuple [IR.ELit (IR.LitInt 1), IR.ELit (IR.LitString "two")])
    , ("ETuple-triple", IR.ETuple [IR.ELit (IR.LitInt 1), IR.ELit (IR.LitInt 2), IR.ELit (IR.LitInt 3)])
    , ("EList-empty", IR.EList [])
    , ("EList-singleton", IR.EList [IR.ELit (IR.LitInt 1)])
    , ("EList-multi", IR.EList [IR.ELit (IR.LitInt 1), IR.ELit (IR.LitInt 2), IR.ELit (IR.LitInt 3)])
    , ("ERaise", IR.ERaise (IR.ELit (IR.LitString "error")))
    , ("EUnreachable", IR.EUnreachable)
    , ( "Nested-let-in-case-in-lambda"
      , IR.ELam [IR.LamParam (n "x") Nothing]
            ( IR.ECase (IR.EVar (n "x"))
                [ IR.Branch (IR.PatCon (qn "Just") [IR.PatBinder (n "v") Nothing])
                    ( IR.ELet
                        [IR.LetBind (n "y") Nothing (IR.EApp (IR.EVar (n "f")) [IR.EVar (n "v")])]
                        (IR.EVar (n "y"))
                    )
                , IR.Branch IR.PatWild (IR.ELit (IR.LitInt 0))
                ]
            )
      )
    , ( "Nested-handle-perform"
      , IR.EHandle (qn "State")
            (IR.EPerform (qn "State") "get" [])
            (IR.ELam [IR.LamParam (n "s") Nothing] (IR.EVar (n "s")))
      )
    , ("Nested-delay-force", IR.EDelay (IR.EForce (IR.EVar (n "thunk"))))
    , ( "Nested-reuse-retain-drop"
      , IR.ELet
            [ IR.LetBind (n "r") Nothing (IR.EReuse (n "old"))
            , IR.LetBind (n "tmp") Nothing (IR.ERetain (n "shared"))
            ]
            (IR.EDrop (n "garbage"))
      )
    ]

-- --------------------------------------------------------------------------
-- Type tests
-- --------------------------------------------------------------------------

typeTests :: [(String, IR.Ty)]
typeTests =
    [ ("TAny", IR.TAny)
    , ("TCon", IR.TCon (qn "Int"))
    , ("TCon-qualified", IR.TCon (qnMod "GHC.Types" "Int"))
    , ("TApp-single", IR.TApp (qn "Maybe") [IR.TCon (qn "Int")])
    , ("TApp-multi", IR.TApp (qn "Either") [IR.TCon (qn "String"), IR.TCon (qn "Int")])
    , ("TVar", IR.TVar (n "a"))
    , ("TVar-unique", IR.TVar (IR.Name "a" 7))
    , ("TFn-pure-no-mult", IR.TFn [IR.FnArg Nothing IR.TAny] (IR.EffectRow [] Nothing) IR.TAny)
    , ("TFn-Many", IR.TFn [IR.FnArg (Just IR.Many) (IR.TCon (qn "Int"))] (IR.EffectRow [] Nothing) (IR.TCon (qn "Int")))
    , ("TFn-Affine", IR.TFn [IR.FnArg (Just IR.Affine) (IR.TCon (qn "Handle"))] (IR.EffectRow [] Nothing) IR.TAny)
    , ("TFn-Linear", IR.TFn [IR.FnArg (Just IR.Linear) (IR.TCon (qn "File"))] (IR.EffectRow [] Nothing) IR.TAny)
    , ( "TFn-multi-args"
      , IR.TFn
            [ IR.FnArg Nothing (IR.TCon (qn "Int"))
            , IR.FnArg (Just IR.Many) (IR.TCon (qn "String"))
            ]
            (IR.EffectRow [] Nothing)
            (IR.TCon (qn "Bool"))
      )
    , ("TFn-with-effects", IR.TFn [IR.FnArg Nothing IR.TAny] (IR.EffectRow [qn "IO", qn "State"] Nothing) IR.TAny)
    , ("TFn-with-effect-tail", IR.TFn [IR.FnArg Nothing IR.TAny] (IR.EffectRow [qn "IO"] (Just (n "e"))) IR.TAny)
    , ("TFn-pure-empty-effects", IR.TFn [IR.FnArg Nothing IR.TAny] (IR.EffectRow [] Nothing) IR.TAny)
    , ("TForall-simple", IR.TForall [IR.TyVar (n "a") Nothing] (IR.TVar (n "a")))
    , ("TForall-with-kind", IR.TForall [IR.TyVar (n "a") (Just "Type")] (IR.TVar (n "a")))
    , ( "TForall-multi"
      , IR.TForall
            [ IR.TyVar (n "a") Nothing
            , IR.TyVar (n "b") (Just "Type -> Type")
            ]
            (IR.TApp (qn "b") [IR.TVar (n "a")])
      )
    , ("TSyn", IR.TSyn (qn "String") (IR.TApp (qn "List") [IR.TCon (qn "Char")]))
    , ( "TFn-nested-result"
      , IR.TFn
            [IR.FnArg Nothing (IR.TCon (qn "Int"))]
            (IR.EffectRow [] Nothing)
            (IR.TFn [IR.FnArg Nothing (IR.TCon (qn "Int"))] (IR.EffectRow [] Nothing) (IR.TCon (qn "Int")))
      )
    , ( "TForall-TFn"
      , IR.TForall [IR.TyVar (n "a") Nothing]
            (IR.TFn [IR.FnArg Nothing (IR.TVar (n "a"))] (IR.EffectRow [] Nothing) (IR.TVar (n "a")))
      )
    ]

-- --------------------------------------------------------------------------
-- SourceLang tests
-- --------------------------------------------------------------------------

langTests :: [(String, IR.SourceLang)]
langTests =
    [ ("LHaskell", IR.LHaskell), ("LRust", IR.LRust), ("LMercury", IR.LMercury)
    , ("LIdris2", IR.LIdris2), ("LLean4", IR.LLean4), ("LKoka", IR.LKoka)
    , ("LOCaml", IR.LOCaml), ("LSwift", IR.LSwift), ("LErlang", IR.LErlang)
    , ("LPurescript", IR.LPurescript), ("LAgda", IR.LAgda), ("LFSharp", IR.LFSharp)
    , ("LScala3", IR.LScala3), ("LJulia", IR.LJulia), ("LZig", IR.LZig)
    , ("LC", IR.LC), ("LCpp", IR.LCpp), ("LFortran", IR.LFortran)
    , ("LAda", IR.LAda), ("LSml", IR.LSml), ("LCommonLisp", IR.LCommonLisp)
    , ("LScheme", IR.LScheme), ("LProlog", IR.LProlog), ("LLua", IR.LLua)
    , ("LForth", IR.LForth)
    ]

-- --------------------------------------------------------------------------
-- Structure tests
-- --------------------------------------------------------------------------

structTests :: [(String, IR.OrganIR)]
structTests =
    [ ( "DataType-with-constructors"
      , IR.OrganIR
            { IR.irMetadata = IR.Metadata IR.LHaskell Nothing Nothing "test-0.1" Nothing
            , IR.irModule = IR.Module "Test" [] []
                [IR.valDef "x" IR.TAny (IR.ELit (IR.LitInt 0))]
                [ IR.DataType (qn "Maybe")
                    [IR.TyVar (n "a") Nothing]
                    [ IR.Constructor (qn "Nothing") []
                    , IR.Constructor (qn "Just") [IR.TVar (n "a")]
                    ]
                , IR.DataType (qn "Bool") []
                    [ IR.Constructor (qn "True") []
                    , IR.Constructor (qn "False") []
                    ]
                ]
                []
            }
      )
    , ( "EffectDecl-with-ops"
      , IR.OrganIR
            { IR.irMetadata = IR.Metadata IR.LKoka Nothing Nothing "test-0.1" Nothing
            , IR.irModule = IR.Module "Test" [] []
                [IR.valDef "x" IR.TAny (IR.ELit (IR.LitInt 0))]
                []
                [ IR.EffectDecl (qn "State")
                    [IR.TyVar (n "s") Nothing]
                    [ IR.Operation "get"
                        (IR.TFn [] (IR.EffectRow [qn "State"] Nothing) (IR.TVar (n "s")))
                    , IR.Operation "put"
                        (IR.TFn [IR.FnArg Nothing (IR.TVar (n "s"))] (IR.EffectRow [qn "State"] Nothing) IR.TAny)
                    ]
                ]
            }
      )
    , ( "Module-with-exports"
      , IR.OrganIR
            { IR.irMetadata = IR.Metadata IR.LHaskell Nothing Nothing "test-0.1" Nothing
            , IR.irModule = IR.Module "MyModule" ["foo", "bar", "baz"] []
                [ IR.funDef "foo" (IR.TFn [IR.FnArg Nothing IR.TAny] (IR.EffectRow [] Nothing) IR.TAny) (IR.ELam [IR.LamParam (n "x") Nothing] (IR.EVar (n "x"))) 1
                , IR.valDef "bar" IR.TAny (IR.ELit (IR.LitInt 42))
                , IR.valDef "baz" IR.TAny (IR.ELit (IR.LitString "hello"))
                ]
                [] []
            }
      )
    , ( "Module-with-imports"
      , IR.OrganIR
            { IR.irMetadata = IR.Metadata IR.LHaskell Nothing Nothing "test-0.1" Nothing
            , IR.irModule = IR.Module "MyModule" []
                [qnMod "Data.List" "map", qnMod "Data.Maybe" "fromMaybe", qnMod "Prelude" "putStrLn"]
                [IR.valDef "x" IR.TAny (IR.ELit (IR.LitInt 42))]
                [] []
            }
      )
    , ( "Metadata-all-fields"
      , IR.OrganIR
            { IR.irMetadata = IR.Metadata IR.LRust (Just "rustc-1.75.0") (Just "src/main.rs") "test-0.1" (Just "2026-03-25T10:30:00Z")
            , IR.irModule = IR.Module "Main" [] []
                [IR.valDef "x" IR.TAny (IR.ELit (IR.LitInt 0))]
                [] []
            }
      )
    , ( "Combined-full-module"
      , IR.OrganIR
            { IR.irMetadata = IR.Metadata IR.LHaskell (Just "ghc-9.14.1") (Just "Test.hs") "test-0.1" (Just "2026-03-25T12:00:00Z")
            , IR.irModule = IR.Module "Test" ["main", "helper"]
                [qnMod "GHC.IO" "putStrLn"]
                [ IR.funDef "main" (IR.TFn [IR.FnArg Nothing IR.TAny] (IR.EffectRow [qn "IO"] Nothing) IR.TAny)
                    (IR.ELam [IR.LamParam (n "args") Nothing]
                        (IR.EHandle (qn "IO")
                            (IR.EPerform (qn "IO") "println" [IR.ELit (IR.LitString "hello")])
                            (IR.EVar (n "ioHandler"))))
                    1
                , IR.valDef "helper" IR.TAny (IR.ELit (IR.LitInt 42))
                ]
                [ IR.DataType (qn "List")
                    [IR.TyVar (n "a") Nothing]
                    [ IR.Constructor (qn "Nil") []
                    , IR.Constructor (qn "Cons") [IR.TVar (n "a"), IR.TApp (qn "List") [IR.TVar (n "a")]]
                    ]
                ]
                [ IR.EffectDecl (qn "IO") []
                    [ IR.Operation "println" (IR.TFn [IR.FnArg Nothing (IR.TCon (qn "String"))] (IR.EffectRow [qn "IO"] Nothing) IR.TAny)
                    ]
                ]
            }
      )
    , ( "Name-unique-suffix"
      , IR.OrganIR
            { IR.irMetadata = IR.Metadata IR.LHaskell Nothing Nothing "test-0.1" Nothing
            , IR.irModule = IR.Module "Test" [] []
                [ IR.Definition (IR.QName "" (IR.Name "x" 42)) IR.TAny (IR.EVar (IR.Name "y" 99)) IR.SVal IR.Public 0 ]
                [] []
            }
      )
    ]
