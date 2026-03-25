-- | Fuzz tests for the OrganIR parser and schema validator.
-- The key property: every input produces either a clean error or a valid
-- result. Nothing ever crashes.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Json (renderOrganIR)
import OrganIR.Parse (JVal (..), maxNestingDepth, parseJSON, parseOrganIR)
import OrganIR.Schema (schemaCheck)
import OrganIR.Types
import OrganIR.Validate (validateOrganIR)
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck

-- --------------------------------------------------------------------------
-- A. Malformed JSON: explicit test cases
-- --------------------------------------------------------------------------

-- | Run a single named test. Returns True on success, False on failure.
runTest :: String -> IO Bool -> IO Bool
runTest label action = do
    ok <- action
    putStrLn $ (if ok then "  PASS: " else "  FAIL: ") <> label
    pure ok

-- | Assert that parsing fails (returns Left).
parseFails :: Text -> IO Bool
parseFails input = do
    result <- try @SomeException (evaluate (parseOrganIR input))
    pure $ case result of
        Left _ -> False -- exception = crash = FAIL
        Right (Left _) -> True -- clean error = PASS
        Right (Right _) -> False -- unexpected success = FAIL

-- | Assert that parsing does not crash (Left or Right both acceptable).
parseNoCrash :: Text -> IO Bool
parseNoCrash input = do
    result <- try @SomeException (evaluate (parseOrganIR input))
    pure $ case result of
        Left _ -> False -- exception = crash = FAIL
        Right _ -> True

-- | Assert that JSON parsing does not crash.
jsonParseNoCrash :: Text -> IO Bool
jsonParseNoCrash input = do
    result <- try @SomeException (evaluate (parseJSON input))
    pure $ case result of
        Left _ -> False
        Right _ -> True

-- | Assert that schema check on a JVal does not crash.
schemaNoCrash :: JVal -> IO Bool
schemaNoCrash jv = do
    result <- try @SomeException (evaluate (length (schemaCheck jv)))
    pure $ case result of
        Left _ -> False
        Right _ -> True

malformedTests :: IO Bool
malformedTests = do
    putStrLn "--- Malformed JSON tests ---"
    results <- sequence
        -- Empty / trivial
        [ runTest "empty string" $ parseFails ""
        , runTest "empty object" $ parseFails "{}"
        , runTest "null" $ parseFails "null"
        , runTest "bare number" $ parseFails "42"
        , runTest "bare string" $ parseFails "\"hello\""
        , runTest "bare array" $ parseFails "[]"

        -- Missing required fields
        , runTest "missing metadata" $
            parseFails "{\"module\": {}}"
        , runTest "missing module" $
            parseFails "{\"metadata\": {}}"
        , runTest "metadata but no definitions in module" $
            parseFails "{\"metadata\": {\"source_language\": \"haskell\", \"shim_version\": \"1.0\"}, \"module\": {\"name\": \"M\"}}"

        -- Wrong types
        , runTest "string where object expected" $
            parseFails "{\"metadata\": \"wrong\", \"module\": \"wrong\"}"
        , runTest "number where array expected" $
            parseFails "{\"metadata\": {\"source_language\": \"haskell\", \"shim_version\": \"1.0\"}, \"module\": {\"name\": \"M\", \"definitions\": 42}}"

        -- Invalid enum values
        , runTest "invalid source_language" $
            parseFails "{\"metadata\": {\"source_language\": \"brainfuck\", \"shim_version\": \"1.0\"}, \"module\": {\"name\": \"M\", \"definitions\": []}}"

        -- Invalid expression tag
        , runTest "invalid expr tag" $
            parseFails $ renderMinimalWithExpr "{\"eunknown\": {}}"

        -- Deeply nested expressions (depth limit)
        , runTest "deeply nested JSON (200+ levels)" $ do
            let deep = T.replicate (maxNestingDepth + 10) "[" <> "1" <> T.replicate (maxNestingDepth + 10) "]"
            parseFails deep

        -- Very long strings (100KB+)
        , runTest "100KB+ string" $ do
            let bigStr = "\"" <> T.replicate 102400 "a" <> "\""
            jsonParseNoCrash bigStr

        -- Unicode edge cases
        , runTest "emoji in names" $
            parseNoCrash "{\"metadata\": {\"source_language\": \"haskell\", \"shim_version\": \"1.0\"}, \"module\": {\"name\": \"\x1F600\", \"definitions\": []}}"
        , runTest "null byte in string" $
            jsonParseNoCrash "{\"key\": \"val\\u0000ue\"}"
        , runTest "BOM prefix" $
            parseNoCrash ("\xFEFF" <> "{}")

        -- Duplicate keys
        , runTest "duplicate keys" $
            parseNoCrash "{\"metadata\": {}, \"metadata\": {}}"

        -- Invalid JSON syntax
        , runTest "trailing comma" $
            parseFails "{\"a\": 1,}"
        , runTest "comments" $
            parseFails "// comment\n{}"
        , runTest "single quotes" $
            parseFails "{'a': 1}"

        -- Numbers
        , runTest "very large integer" $
            jsonParseNoCrash (T.pack (show (10 ^ (1000 :: Integer))))
        , runTest "NaN text" $
            jsonParseNoCrash "NaN"
        , runTest "Infinity text" $
            jsonParseNoCrash "Infinity"

        -- Escape sequences
        , runTest "invalid \\uXXXX" $
            jsonParseNoCrash "\"\\uZZZZ\""
        , runTest "lone high surrogate" $
            jsonParseNoCrash "\"\\uD800\""
        , runTest "truncated \\u escape" $
            jsonParseNoCrash "\"\\u00\""

        -- Schema check on non-object
        , runTest "schemaCheck on JNull" $ schemaNoCrash JNull
        , runTest "schemaCheck on JStr" $ schemaNoCrash (JStr "hello")
        , runTest "schemaCheck on JArr" $ schemaNoCrash (JArr [])
        , runTest "schemaCheck on JInt" $ schemaNoCrash (JInt 42)
        , runTest "schemaCheck on nested garbage" $
            schemaNoCrash (JObj [("metadata", JArr [JNull]), ("module", JBool True)])
        ]
    pure (and results)

-- | Build a minimal OrganIR JSON document but with a custom expr value.
renderMinimalWithExpr :: Text -> Text
renderMinimalWithExpr exprJson = T.concat
    [ "{\"metadata\":{\"source_language\":\"haskell\",\"shim_version\":\"1.0\"}"
    , ",\"module\":{\"name\":\"M\",\"definitions\":[{\"name\":{\"module\":\"M\""
    , ",\"name\":{\"text\":\"f\",\"unique\":0}},\"type\":{\"con\":{\"qname\":"
    , "{\"module\":\"\",\"name\":{\"text\":\"any\",\"unique\":0}}}}"
    , ",\"expr\":", exprJson
    , ",\"sort\":\"fun\",\"visibility\":\"public\",\"arity\":0}]}}"
    ]

-- --------------------------------------------------------------------------
-- B. QuickCheck fuzz properties
-- --------------------------------------------------------------------------

-- | Newtype for generating arbitrary text (including garbage).
newtype FuzzText = FuzzText { unFuzzText :: Text }
    deriving (Show)

instance Arbitrary FuzzText where
    arbitrary = FuzzText . T.pack <$> listOf genFuzzChar
      where
        genFuzzChar = frequency
            [ (10, elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
            , (5, elements ['{', '}', '[', ']', '"', ':', ',', ' ', '\n', '\t'])
            , (2, elements ['\\', '/', '.', '-', '+', '_'])
            , (1, elements ['\0', '\xFEFF', '\x1F600'])
            , (1, arbitrary)
            ]

-- | parseOrganIR on arbitrary text never throws an exception.
prop_parseNeverCrashes :: FuzzText -> Property
prop_parseNeverCrashes (FuzzText input) = ioProperty $ do
    result <- try @SomeException (evaluate (parseOrganIR input))
    pure $ case result of
        Left ex -> counterexample ("Exception: " ++ show ex) False
        Right (Left _) -> property True
        Right (Right _) -> property True

-- | parseJSON on arbitrary text never throws an exception.
prop_jsonParseNeverCrashes :: FuzzText -> Property
prop_jsonParseNeverCrashes (FuzzText input) = ioProperty $ do
    result <- try @SomeException (evaluate (parseJSON input))
    pure $ case result of
        Left ex -> counterexample ("Exception: " ++ show ex) False
        Right _ -> property True

-- | schemaCheck on any parseable JSON value never crashes.
prop_schemaCheckNeverCrashes :: FuzzText -> Property
prop_schemaCheckNeverCrashes (FuzzText input) = ioProperty $ do
    case parseJSON input of
        Left _ -> pure (property True) -- can't parse, nothing to check
        Right jv -> do
            result <- try @SomeException (evaluate (length (schemaCheck jv)))
            pure $ case result of
                Left ex -> counterexample ("Exception: " ++ show ex) False
                Right _ -> property True

-- | validateOrganIR on any well-typed OrganIR never crashes.
prop_validateNeverCrashes :: OrganIR -> Property
prop_validateNeverCrashes ir = ioProperty $ do
    result <- try @SomeException (evaluate (length (validateOrganIR ir)))
    pure $ case result of
        Left ex -> counterexample ("Exception: " ++ show ex) False
        Right _ -> property True

-- | schemaCheck on any JVal never crashes.
prop_schemaCheckJValNeverCrashes :: JVal -> Property
prop_schemaCheckJValNeverCrashes jv = ioProperty $ do
    result <- try @SomeException (evaluate (length (schemaCheck jv)))
    pure $ case result of
        Left ex -> counterexample ("Exception: " ++ show ex) False
        Right _ -> property True

-- --------------------------------------------------------------------------
-- Arbitrary instances for fuzzing
-- --------------------------------------------------------------------------

-- | Generate random JSON-like text.
instance Arbitrary JVal where
    arbitrary = sized genJVal
    shrink = \case
        JObj fs -> JNull : [JObj fs' | fs' <- shrinkList shrinkField fs]
        JArr xs -> JNull : [JArr xs' | xs' <- shrinkList shrink xs]
        JStr _ -> [JNull]
        JInt n -> JNull : [JInt n' | n' <- shrink n]
        JFloat _ -> [JNull, JFloat 0.0]
        JBool _ -> [JNull]
        JNull -> []
      where
        shrinkField (k, v) = [(k, v') | v' <- shrink v]

genJVal :: Int -> Gen JVal
genJVal 0 = oneof
    [ JStr <$> genSafeString
    , JInt <$> arbitrary
    , JFloat <$> arbitrary
    , JBool <$> arbitrary
    , pure JNull
    ]
genJVal n = frequency
    [ (3, JStr <$> genSafeString)
    , (3, JInt <$> arbitrary)
    , (2, JFloat <$> arbitrary)
    , (2, JBool <$> arbitrary)
    , (2, pure JNull)
    , (2, JObj <$> resize 5 (listOf ((,) <$> genIdent <*> genJVal n')))
    , (2, JArr <$> resize 5 (listOf (genJVal n')))
    ]
  where n' = n `div` 2

-- Reuse Arbitrary instances from PropertyRoundTrip via orphans.
-- We need our own since this is a separate module.

genIdentChar :: Gen Char
genIdentChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

genIdent :: Gen Text
genIdent = do
    c <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
    rest <- listOf genIdentChar
    pure $ T.pack (c : rest)

genSafeString :: Gen Text
genSafeString = T.pack <$> listOf genSafeChar
  where
    genSafeChar = frequency
        [ (10, elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '_', '-', '.', ',', '!', '?'])
        , (1, elements ['\n', '\t', '\\', '"'])
        ]

instance Arbitrary Name where
    arbitrary = Name <$> genIdent <*> (getNonNegative <$> arbitrary)

instance Arbitrary QName where
    arbitrary = QName <$> frequency [(3, pure ""), (1, genIdent)] <*> arbitrary

instance Arbitrary SourceLang where
    arbitrary = elements
        [ LHaskell, LRust, LMercury, LIdris2, LLean4, LKoka, LOCaml, LSwift
        , LErlang, LPurescript, LAgda, LFSharp, LScala3, LJulia, LZig
        , LC, LCpp, LFortran, LAda, LSml, LCommonLisp, LScheme, LProlog
        , LLua, LForth
        ]

instance Arbitrary Multiplicity where
    arbitrary = elements [Many, Affine, Linear]

instance Arbitrary Sort where
    arbitrary = elements [SFun, SVal, SExternal, SCon]

instance Arbitrary Visibility where
    arbitrary = elements [Public, Private]

genTy :: Int -> Gen Ty
genTy 0 = oneof [pure TAny, TVar <$> arbitrary, TCon <$> arbitrary]
genTy n = frequency
    [ (3, pure TAny)
    , (3, TVar <$> arbitrary)
    , (3, TCon <$> arbitrary)
    , (2, TApp <$> arbitrary <*> listOf1 (genTy n'))
    , (2, TFn <$> listOf (genFnArg n') <*> genEffectRow <*> genTy n')
    , (1, TForall <$> listOf1 genTyVar <*> genTy n')
    , (1, TSyn <$> arbitrary <*> genTy n')
    ]
  where n' = n `div` 2

instance Arbitrary Ty where
    arbitrary = sized $ \s -> genTy (min s 4)

genFnArg :: Int -> Gen FnArg
genFnArg depth = FnArg <$> arbitrary <*> genTy depth

instance Arbitrary FnArg where
    arbitrary = sized $ \s -> genFnArg (min s 3)

genTyVar :: Gen TyVar
genTyVar = TyVar <$> arbitrary <*> frequency [(3, pure Nothing), (1, Just <$> genIdent)]

instance Arbitrary TyVar where
    arbitrary = genTyVar

genEffectRow :: Gen EffectRow
genEffectRow = EffectRow
    <$> resize 3 (listOf arbitrary)
    <*> frequency [(3, pure Nothing), (1, Just <$> arbitrary)]

instance Arbitrary EffectRow where
    arbitrary = genEffectRow

instance Arbitrary Lit where
    arbitrary = oneof
        [ LitInt <$> arbitrary
        , LitFloat <$> genFiniteDouble
        , LitString <$> genSafeString
        , LitBool <$> arbitrary
        ]
      where
        genFiniteDouble = do
            d <- arbitrary
            if isNaN d || isInfinite d then pure 0.0 else pure d

genExpr :: Int -> Gen Expr
genExpr 0 = oneof [EVar <$> arbitrary, ELit <$> arbitrary, pure EUnreachable]
genExpr n = frequency
    [ (5, EVar <$> arbitrary)
    , (5, ELit <$> arbitrary)
    , (2, ECon <$> arbitrary <*> resize 3 (listOf (genExpr n')))
    , (2, EApp <$> genExpr n' <*> listOf1 (genExpr n'))
    , (2, ELam <$> listOf1 (genLamParam n') <*> genExpr n')
    , (2, ELet <$> listOf1 (genLetBind n') <*> genExpr n')
    , (2, ECase <$> genExpr n' <*> listOf1 (genBranch n'))
    , (1, ETypeApp <$> genExpr n' <*> listOf1 (genTy n'))
    , (1, ETypeLam <$> listOf1 genTyVar <*> genExpr n')
    , (1, EPerform <$> arbitrary <*> genIdent <*> resize 3 (listOf (genExpr n')))
    , (1, EHandle <$> arbitrary <*> genExpr n' <*> genExpr n')
    , (1, ERetain <$> arbitrary)
    , (1, ERelease <$> arbitrary)
    , (1, EDrop <$> arbitrary)
    , (1, EReuse <$> arbitrary)
    , (1, EDelay <$> genExpr n')
    , (1, EForce <$> genExpr n')
    , (1, ETuple <$> resize 4 (listOf (genExpr n')))
    , (1, EList <$> resize 4 (listOf (genExpr n')))
    , (1, ERaise <$> genExpr n')
    , (1, pure EUnreachable)
    ]
  where n' = n `div` 2

instance Arbitrary Expr where
    arbitrary = sized $ \s -> genExpr (min s 4)

genLamParam :: Int -> Gen LamParam
genLamParam depth = LamParam <$> arbitrary <*> frequency [(2, pure Nothing), (1, Just <$> genTy depth)]

instance Arbitrary LamParam where
    arbitrary = sized $ \s -> genLamParam (min s 3)

genLetBind :: Int -> Gen LetBind
genLetBind depth = LetBind <$> arbitrary <*> frequency [(2, pure Nothing), (1, Just <$> genTy depth)] <*> genExpr depth

instance Arbitrary LetBind where
    arbitrary = sized $ \s -> genLetBind (min s 3)

genBranch :: Int -> Gen Branch
genBranch depth = Branch <$> genPat depth <*> genExpr depth

instance Arbitrary Branch where
    arbitrary = sized $ \s -> genBranch (min s 3)

genPat :: Int -> Gen Pat
genPat depth = oneof
    [ PatCon <$> arbitrary <*> resize 4 (listOf (genPatBinder depth))
    , PatLit <$> arbitrary
    , PatVar <$> arbitrary <*> frequency [(2, pure Nothing), (1, Just <$> genTy depth)]
    , pure PatWild
    ]

instance Arbitrary Pat where
    arbitrary = sized $ \s -> genPat (min s 3)

genPatBinder :: Int -> Gen PatBinder
genPatBinder depth = PatBinder <$> arbitrary <*> frequency [(2, pure Nothing), (1, Just <$> genTy depth)]

instance Arbitrary PatBinder where
    arbitrary = sized $ \s -> genPatBinder (min s 3)

instance Arbitrary Constructor where
    arbitrary = Constructor <$> arbitrary <*> resize 4 (listOf (sized $ \s -> genTy (min s 3)))

instance Arbitrary DataType where
    arbitrary = DataType <$> arbitrary <*> resize 3 (listOf genTyVar) <*> resize 4 (listOf arbitrary)

instance Arbitrary Operation where
    arbitrary = Operation <$> genIdent <*> (sized $ \s -> genTy (min s 3))

instance Arbitrary EffectDecl where
    arbitrary = EffectDecl <$> arbitrary <*> resize 3 (listOf genTyVar) <*> resize 4 (listOf arbitrary)

instance Arbitrary Definition where
    arbitrary = Definition
        <$> arbitrary
        <*> (sized $ \s -> genTy (min s 3))
        <*> (sized $ \s -> genExpr (min s 3))
        <*> arbitrary
        <*> arbitrary
        <*> (getNonNegative <$> arbitrary)

instance Arbitrary Module where
    arbitrary = Module
        <$> genIdent
        <*> resize 3 (listOf genIdent)
        <*> resize 2 (listOf arbitrary)
        <*> resize 3 (listOf arbitrary)
        <*> resize 2 (listOf arbitrary)
        <*> resize 2 (listOf arbitrary)

instance Arbitrary Metadata where
    arbitrary = Metadata
        <$> arbitrary
        <*> frequency [(2, pure Nothing), (1, Just <$> genIdent)]
        <*> frequency [(2, pure Nothing), (1, Just <$> genIdent)]
        <*> genIdent
        <*> frequency [(2, pure Nothing), (1, Just <$> genIdent)]

instance Arbitrary OrganIR where
    arbitrary = OrganIR <$> arbitrary <*> arbitrary

-- --------------------------------------------------------------------------
-- Main
-- --------------------------------------------------------------------------

main :: IO ()
main = do
    -- Part A: explicit malformed tests
    putStrLn "=== Malformed JSON tests ==="
    malformedOk <- malformedTests

    -- Part B: QuickCheck fuzz properties
    putStrLn "\n=== QuickCheck fuzz: parseOrganIR never crashes ==="
    r1 <- quickCheckResult (withNumTests 5000 prop_parseNeverCrashes)

    putStrLn "\n=== QuickCheck fuzz: parseJSON never crashes ==="
    r2 <- quickCheckResult (withNumTests 5000 prop_jsonParseNeverCrashes)

    putStrLn "\n=== QuickCheck fuzz: schemaCheck on parsed JSON never crashes ==="
    r3 <- quickCheckResult (withNumTests 5000 prop_schemaCheckNeverCrashes)

    putStrLn "\n=== QuickCheck fuzz: validateOrganIR never crashes ==="
    r4 <- quickCheckResult (withNumTests 5000 prop_validateNeverCrashes)

    putStrLn "\n=== QuickCheck fuzz: schemaCheck on random JVal never crashes ==="
    r5 <- quickCheckResult (withNumTests 5000 prop_schemaCheckJValNeverCrashes)

    let allQC = all qcSuccess [r1, r2, r3, r4, r5]
    putStrLn $ "\n" <> if malformedOk && allQC then "All fuzz tests passed." else "SOME FUZZ TESTS FAILED."
    if malformedOk && allQC then exitSuccess else exitFailure

qcSuccess :: Result -> Bool
qcSuccess Success{} = True
qcSuccess _ = False
