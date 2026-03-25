module Main (main) where

import OrganIR.Build
import OrganIR.Transform
import OrganIR.Types
import System.Exit (exitFailure, exitSuccess)

-- | Helper: wrap definitions into a minimal OrganIR document.
mkIR :: [Definition] -> OrganIR
mkIR = organIR LHaskell "test" "Test"

-- | Check a condition, print a message, return success/failure.
check :: String -> Bool -> IO Bool
check label True  = putStrLn ("  PASS: " ++ label) >> pure True
check label False = putStrLn ("  FAIL: " ++ label) >> pure False

-- | Extract definition bodies from an IR.
bodies :: OrganIR -> [Expr]
bodies = map defExpr . modDefs . irModule

-- | Extract definition names from an IR.
defNames :: OrganIR -> [Name]
defNames = map (qnName . defName) . modDefs . irModule

main :: IO ()
main = do
    putStrLn "=== OrganIR.Transform tests ==="

    results <- sequence
        [ testConstantFoldInt
        , testConstantFoldFloat
        , testConstantFoldBoolCase
        , testDeadDefElim
        , testInlineTrivialVar
        , testInlineTrivialLit
        , testSimplifyCaseWild
        , testSimplifyCaseKnownCon
        , testEtaReduce
        , testEtaReduceNoCapture
        , testRunPasses
        , testMapDefs
        ]

    if and results
        then putStrLn "\nAll tests passed." >> exitSuccess
        else putStrLn "\nSome tests FAILED." >> exitFailure

-- * Constant folding

testConstantFoldInt :: IO Bool
testConstantFoldInt = do
    putStrLn "\n-- constantFold (integer)"
    let ir = mkIR [valDefSimple "x" (EApp (eVar "+") [eInt 3, eInt 4])]
        result = constantFold ir
    check "3 + 4 = 7" $ case bodies result of
        [ELit (LitInt 7)] -> True
        _                 -> False

testConstantFoldFloat :: IO Bool
testConstantFoldFloat = do
    putStrLn "\n-- constantFold (float)"
    let ir = mkIR [valDefSimple "x" (EApp (eVar "*") [eFloat 2.5, eFloat 4.0])]
        result = constantFold ir
    check "2.5 * 4.0 = 10.0" $ case bodies result of
        [ELit (LitFloat f)] -> f == 10.0
        _                   -> False

testConstantFoldBoolCase :: IO Bool
testConstantFoldBoolCase = do
    putStrLn "\n-- constantFold (bool case)"
    -- ECase (LitBool True) [Branch (PatCon "true" []) body, ...]
    let body = eInt 42
        ir = mkIR [valDefSimple "x" (eIf (eBool True) body (eInt 99))]
        result = constantFold ir
    check "case True of {true -> 42; false -> 99} = 42" $ case bodies result of
        [ELit (LitInt 42)] -> True
        _                  -> False

-- * Dead definition elimination

testDeadDefElim :: IO Bool
testDeadDefElim = do
    putStrLn "\n-- deadDefElim"
    let pubDef = (funDef "main" TAny (eApp (eVar "helper") [eInt 1]) 1)
        helperDef = (funDef "helper" TAny (eVar "x") 1){defVisibility = Private}
        deadDef = (valDefSimple "dead" (eInt 999)){defVisibility = Private}
        ir = mkIR [pubDef, helperDef, deadDef]
        result = deadDefElim ir
        names = map nameText (defNames result)
    r1 <- check "keeps public def 'main'" ("main" `elem` names)
    r2 <- check "keeps referenced private def 'helper'" ("helper" `elem` names)
    r3 <- check "removes unreferenced private def 'dead'" ("dead" `notElem` names)
    pure (r1 && r2 && r3)

-- * Inline trivial

testInlineTrivialVar :: IO Bool
testInlineTrivialVar = do
    putStrLn "\n-- inlineTrivial (variable)"
    let alias = (valDefSimple "alias" (eVar "real")){defVisibility = Private}
        user = valDefSimple "user" (eApp (eVar "alias") [eInt 1])
        ir = mkIR [alias, user]
        result = inlineTrivial ir
        names = map nameText (defNames result)
    r1 <- check "removes trivial alias def" ("alias" `notElem` names)
    r2 <- check "substitutes alias -> real" $ case bodies result of
        [EApp (EVar n) _] -> nameText n == "real"
        _                 -> False
    pure (r1 && r2)

testInlineTrivialLit :: IO Bool
testInlineTrivialLit = do
    putStrLn "\n-- inlineTrivial (literal)"
    let constDef = (valDefSimple "c" (eInt 42)){defVisibility = Private}
        user = valDefSimple "user" (eApp (eVar "+") [eVar "c", eInt 1])
        ir = mkIR [constDef, user]
        result = inlineTrivial ir
        names = map nameText (defNames result)
    r1 <- check "removes trivial const def" ("c" `notElem` names)
    r2 <- check "substitutes c -> 42" $ case bodies result of
        [EApp (EVar _) [ELit (LitInt 42), ELit (LitInt 1)]] -> True
        _ -> False
    pure (r1 && r2)

-- * Simplify case

testSimplifyCaseWild :: IO Bool
testSimplifyCaseWild = do
    putStrLn "\n-- simplifyCase (single wildcard)"
    let ir = mkIR [valDefSimple "x" (ECase (eVar "y") [Branch PatWild (eInt 1)])]
        result = simplifyCase ir
    check "case y of {_ -> 1} = 1" $ case bodies result of
        [ELit (LitInt 1)] -> True
        _                 -> False

testSimplifyCaseKnownCon :: IO Bool
testSimplifyCaseKnownCon = do
    putStrLn "\n-- simplifyCase (known constructor)"
    let ir = mkIR [valDefSimple "x"
                (ECase (ECon (localName "nil") [])
                    [ Branch (PatCon (localName "nil") []) (eInt 1)
                    , Branch PatWild (eInt 2)
                    ])]
        result = simplifyCase ir
    check "case nil of {nil -> 1; _ -> 2} = 1" $ case bodies result of
        [ELit (LitInt 1)] -> True
        _                 -> False

-- * Eta reduction

testEtaReduce :: IO Bool
testEtaReduce = do
    putStrLn "\n-- etaReduce"
    -- \x -> f x  ==>  f
    let ir = mkIR [valDefSimple "g" (ELam [LamParam (Name "x" 0) Nothing] (EApp (eVar "f") [EVar (Name "x" 0)]))]
        result = etaReduce ir
    check "\\x -> f x = f" $ case bodies result of
        [EVar n] -> nameText n == "f"
        _        -> False

testEtaReduceNoCapture :: IO Bool
testEtaReduceNoCapture = do
    putStrLn "\n-- etaReduce (no capture)"
    -- \x -> (x + 1) x  -- x is free in the function part, should NOT reduce
    let xn = Name "x" 0
        ir = mkIR [valDefSimple "g"
                (ELam [LamParam xn Nothing]
                    (EApp (EApp (eVar "+") [EVar xn, eInt 1]) [EVar xn]))]
        result = etaReduce ir
    -- Should remain unchanged (x appears free in function position)
    check "\\x -> (x+1) x unchanged" $ case bodies result of
        [ELam{} ] -> True
        _         -> False

-- * Composition

testRunPasses :: IO Bool
testRunPasses = do
    putStrLn "\n-- runPasses (composition)"
    -- inline trivial, then constant fold
    let constDef = (valDefSimple "c" (eInt 3)){defVisibility = Private}
        user = valDefSimple "user" (EApp (eVar "+") [eVar "c", eInt 4])
        ir = mkIR [constDef, user]
        result = runPasses [inlineTrivial, constantFold] ir
    check "inline then fold: c=3; c+4 => 7" $ case bodies result of
        [ELit (LitInt 7)] -> True
        _                 -> False

testMapDefs :: IO Bool
testMapDefs = do
    putStrLn "\n-- mapDefs"
    -- Use mapDefs to set all arities to 99
    let ir = mkIR [funDef "f" TAny (eVar "x") 1, funDef "g" TAny (eVar "y") 2]
        result = mapDefs (\d -> d{defArity = 99}) ir
        arities = map defArity (modDefs (irModule result))
    check "all arities set to 99" (arities == [99, 99])
