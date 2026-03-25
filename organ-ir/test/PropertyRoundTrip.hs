-- | Property-based round-trip tests for OrganIR using QuickCheck.
-- For every generated OrganIR value: parseOrganIR (renderOrganIR x) round-trips
-- stably (render -> parse -> render produces identical JSON text).
module Main (main) where

import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Json (renderOrganIR)
import OrganIR.Parse (parseOrganIR)
import OrganIR.Types
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck

-- --------------------------------------------------------------------------
-- Arbitrary instances
-- --------------------------------------------------------------------------

-- | Safe characters for identifiers: alphanumeric and underscore.
genIdentChar :: Gen Char
genIdentChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

-- | Non-empty identifier text.
genIdent :: Gen Text
genIdent = do
    c <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
    rest <- listOf genIdentChar
    pure $ T.pack (c : rest)

-- | Safe string content for LitString (avoids control chars that break JSON).
genSafeString :: Gen Text
genSafeString = T.pack <$> listOf genSafeChar
  where
    genSafeChar = frequency
        [ (10, elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '_', '-', '.', ',', '!', '?'])
        , (1, elements ['\n', '\t', '\\', '"'])
        ]

instance Arbitrary Name where
    arbitrary = Name <$> genIdent <*> (getNonNegative <$> arbitrary)
    shrink (Name t u) =
        [Name t' u | t' <- shrinkIdent t] ++
        [Name t u' | u' <- shrink u, u' >= 0]

-- | Shrink an identifier text, keeping it non-empty.
shrinkIdent :: Text -> [Text]
shrinkIdent t
    | T.length t <= 1 = []
    | otherwise = [T.take (T.length t `div` 2) t]

instance Arbitrary QName where
    arbitrary = QName <$> genModuleName <*> arbitrary
      where
        genModuleName = frequency
            [ (3, pure "")
            , (1, genIdent)
            , (1, do
                parts <- listOf1 genIdent
                pure $ T.intercalate "." parts)
            ]
    shrink (QName m n) =
        [QName "" n | m /= ""] ++
        [QName m n' | n' <- shrink n]

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

-- | Generate types with bounded depth.
genTy :: Int -> Gen Ty
genTy 0 = oneof
    [ TAny <$ (pure () :: Gen ())
    , TVar <$> arbitrary
    , TCon <$> arbitrary
    ]
genTy n = frequency
    [ (3, pure TAny)
    , (3, TVar <$> arbitrary)
    , (3, TCon <$> arbitrary)
    , (2, TApp <$> arbitrary <*> listOf1 (genTy n'))
    , (2, TFn <$> listOf (genFnArg n') <*> genEffectRow <*> genTy n')
    , (1, TForall <$> listOf1 (genTyVar) <*> genTy n')
    , (1, TSyn <$> arbitrary <*> genTy n')
    ]
  where n' = n `div` 2

instance Arbitrary Ty where
    arbitrary = sized $ \s -> genTy (min s 4)
    shrink = \case
        TAny -> []
        TVar _ -> [TAny]
        TCon _ -> [TAny]
        TApp _ ts -> TAny : ts
        TFn _ _ ret -> [TAny, ret]
        TForall _ t -> [TAny, t]
        TSyn _ t -> [TAny, t]

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

-- | Generate Lit values. Floats are restricted to finite values that
-- round-trip through show/read.
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
    shrink = \case
        LitInt n -> LitInt <$> shrink n
        LitFloat _ -> [LitFloat 0.0]
        LitString _ -> [LitString ""]
        LitBool _ -> []

-- | Generate expressions with bounded depth.
genExpr :: Int -> Gen Expr
genExpr 0 = oneof
    [ EVar <$> arbitrary
    , ELit <$> arbitrary
    , EUnreachable <$ (pure () :: Gen ())
    ]
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
    shrink = \case
        EVar _ -> []
        ELit l -> ELit <$> shrink l
        EUnreachable -> []
        ECon _ es -> EUnreachable : es
        EApp f xs -> f : xs
        ELam _ e -> [e]
        ELet _ e -> [e]
        ECase e _ -> [e]
        ETypeApp e _ -> [e]
        ETypeLam _ e -> [e]
        EPerform _ _ es -> EUnreachable : es
        EHandle _ e1 e2 -> [e1, e2]
        ERetain _ -> [EUnreachable]
        ERelease _ -> [EUnreachable]
        EDrop _ -> [EUnreachable]
        EReuse _ -> [EUnreachable]
        EDelay e -> [e]
        EForce e -> [e]
        ETuple es -> EUnreachable : es
        EList es -> EUnreachable : es
        ERaise e -> [e]

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
    shrink (OrganIR meta modul) =
        [ OrganIR meta modul{modDefs = take 1 (modDefs modul), modDataTypes = [], modEffectDecls = []}
        | length (modDefs modul) > 1 || not (null (modDataTypes modul)) || not (null (modEffectDecls modul))
        ]

-- --------------------------------------------------------------------------
-- Round-trip property
-- --------------------------------------------------------------------------

-- | The core property: render -> parse -> render produces identical JSON.
prop_roundTrip :: OrganIR -> Property
prop_roundTrip ir =
    let json1 = renderOrganIR ir
    in case parseOrganIR json1 of
        Left err -> counterexample ("Parse failed: " ++ T.unpack err ++ "\nJSON: " ++ T.unpack (T.take 500 json1)) False
        Right ir2 ->
            let json2 = renderOrganIR ir2
            in counterexample
                ("Render mismatch!\nFirst:  " ++ T.unpack (T.take 500 json1) ++ "\nSecond: " ++ T.unpack (T.take 500 json2))
                (json1 === json2)

-- --------------------------------------------------------------------------
-- Main
-- --------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "=== Property-based round-trip tests ==="
    result <- quickCheckResult (withMaxSuccess 1000 prop_roundTrip)
    case result of
        Success{} -> exitSuccess
        _ -> exitFailure
