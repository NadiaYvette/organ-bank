-- | Performance benchmarks for organ-ir: JSON rendering, parsing,
-- validation, pretty-printing, and round-trip.
module Main (main) where

import Control.Exception (evaluate)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.Bench

import OrganIR.Build
import OrganIR.Json (renderOrganIR)
import OrganIR.Parse (parseOrganIR)
import OrganIR.Pretty (ppOrganIR)
import OrganIR.Types
import OrganIR.Validate (validateOrganIR)

-- * Data generators

-- | Generate a module with N factorial-like function definitions.
-- Each definition is: fun fact_i/1 : (Int) -> Int = \(n) -> case n of ...
generateModule :: Int -> OrganIR
generateModule n = organIR LHaskell "bench-0.1" "Bench" (map mkFactDef [0 .. n - 1])

mkFactDef :: Int -> Definition
mkFactDef i =
    let nm = "fact_" <> T.pack (show i)
        ty = tFn [tCon "Int"] (tCon "Int")
        body =
            eLam ["n"] $
                ECase
                    (eVar "n")
                    [ Branch (PatLit (LitInt 0)) (eInt 1)
                    , Branch
                        (pVar "m")
                        ( eApp
                            (eVar "*")
                            [ eVar "n"
                            , eApp (eVar nm) [eApp (eVar "-") [eVar "n", eInt 1]]
                            ]
                        )
                    ]
     in funDef nm ty body 1

-- | Generate a deeply nested expression: let x0 = let x1 = ... in xN in x0
generateDeepExpr :: Int -> Expr
generateDeepExpr depth = go 0
  where
    go i
        | i >= depth = eInt 42
        | otherwise =
            let v = "x_" <> T.pack (show i)
             in eLet1 v (go (i + 1)) (eVar v)

-- | Module with a single definition containing a deeply nested expression.
generateDeepModule :: Int -> OrganIR
generateDeepModule depth =
    organIR LHaskell "bench-0.1" "DeepBench"
        [valDef "deep" tAny (generateDeepExpr depth)]

-- * Forcing helpers (OrganIR types lack NFData instances)

-- | Force a parse result by extracting the module name on success.
forceParse :: Either Text OrganIR -> Either Text Text
forceParse (Left err) = Left err
forceParse (Right ir) = Right (modName (irModule ir))

-- | Force a validate result by counting warnings.
forceValidate :: OrganIR -> Int
forceValidate = length . validateOrganIR

-- | Parse then validate, returning the warning count.
parseAndValidate :: Text -> Either Text Int
parseAndValidate t = case parseOrganIR t of
    Left err -> Left err
    Right ir -> Right (length (validateOrganIR ir))

-- | Render then parse back (round-trip), forcing via module name.
roundtrip :: OrganIR -> Either Text Text
roundtrip = forceParse . parseOrganIR . renderOrganIR

-- * Main

main :: IO ()
main = do
    -- Pre-generate data to avoid measuring generation time
    let mod10 = generateModule 10
        mod100 = generateModule 100
        mod1000 = generateModule 1000
        deepMod = generateDeepModule 50

    -- Pre-render JSON for parse benchmarks
    let json10 = renderOrganIR mod10
        json100 = renderOrganIR mod100
        json1000 = renderOrganIR mod1000
        jsonDeep = renderOrganIR deepMod

    -- Force the pre-generated data so generation cost is excluded
    _ <- evaluate (T.length json10)
    _ <- evaluate (T.length json100)
    _ <- evaluate (T.length json1000)
    _ <- evaluate (T.length jsonDeep)

    defaultMain
        [ bgroup
            "render"
            [ bench "10 defs" $ nf renderOrganIR mod10
            , bench "100 defs" $ nf renderOrganIR mod100
            , bench "1000 defs" $ nf renderOrganIR mod1000
            , bench "deep-50" $ nf renderOrganIR deepMod
            ]
        , bgroup
            "parse"
            [ bench "10 defs" $ nf (forceParse . parseOrganIR) json10
            , bench "100 defs" $ nf (forceParse . parseOrganIR) json100
            , bench "1000 defs" $ nf (forceParse . parseOrganIR) json1000
            , bench "deep-50" $ nf (forceParse . parseOrganIR) jsonDeep
            , bgroup
                "parse+validate"
                [ bench "10 defs" $ nf parseAndValidate json10
                , bench "100 defs" $ nf parseAndValidate json100
                , bench "1000 defs" $ nf parseAndValidate json1000
                ]
            ]
        , bgroup
            "validate"
            [ bench "10 defs" $ nf forceValidate mod10
            , bench "100 defs" $ nf forceValidate mod100
            , bench "1000 defs" $ nf forceValidate mod1000
            ]
        , bgroup
            "pretty"
            [ bench "10 defs" $ nf ppOrganIR mod10
            , bench "100 defs" $ nf ppOrganIR mod100
            , bench "1000 defs" $ nf ppOrganIR mod1000
            ]
        , bgroup
            "roundtrip"
            [ bench "10 defs" $ nf roundtrip mod10
            , bench "100 defs" $ nf roundtrip mod100
            , bench "1000 defs" $ nf roundtrip mod1000
            ]
        ]
