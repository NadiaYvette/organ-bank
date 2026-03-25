module OrganBank.KokaShim (
    extractOrganIR,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.FilePath (takeBaseName)
import System.Process (readProcess)

------------------------------------------------------------------------
-- OrganIR types
------------------------------------------------------------------------

data OrganDef = OrganDef
    { odName :: Text
    , odParams :: [(Text, Text)] -- (name, mapped type)
    , odEffects :: [Text]
    , odReturnType :: Text
    }
    deriving (Show)

data OrganModule = OrganModule
    { omName :: Text
    , omImports :: [Text]
    , omDefs :: [OrganDef]
    }
    deriving (Show)

------------------------------------------------------------------------
-- Public entry point
------------------------------------------------------------------------

-- | Run koka --showcore on a .kk file and return OrganIR JSON.
extractOrganIR :: FilePath -> IO Text
extractOrganIR kkFile = do
    raw <- readProcess "koka" ["--showcore", kkFile] ""
    let coreText = T.pack raw
        modul = parseCoreOutput (T.pack (takeBaseName kkFile)) coreText
    pure (renderOrganIR (moduleToIR modul))

------------------------------------------------------------------------
-- Parsing Koka Core text
------------------------------------------------------------------------

parseCoreOutput :: Text -> Text -> OrganModule
parseCoreOutput fallbackName raw =
    let ls = T.lines raw
        modName = parseModuleName ls `orElse` fallbackName
        imports = parseImports ls
        defs = parseDefs ls
     in OrganModule modName imports defs
  where
    orElse Nothing d = d
    orElse (Just x) _ = x

-- | Extract module name from "module <name>"
parseModuleName :: [Text] -> Maybe Text
parseModuleName [] = Nothing
parseModuleName (l : rest)
    | "module " `T.isPrefixOf` stripped =
        Just (T.strip (T.drop 7 stripped))
    | otherwise = parseModuleName rest
  where
    stripped = T.strip l

-- | Extract import names from "import <alias> = <full> ..."
parseImports :: [Text] -> [Text]
parseImports = foldr go []
  where
    go l acc
        | "import " `T.isPrefixOf` stripped =
            let after = T.strip (T.drop 7 stripped)
                -- "alias = full.name pub = ..."  →  full.name
                parts = T.words after
                imp = case parts of
                    (_alias : "=" : fullName : _) -> T.stripEnd (T.dropWhileEnd (== ';') fullName)
                    (name : _) -> T.stripEnd (T.dropWhileEnd (== ';') name)
                    _ -> after
             in imp : acc
        | otherwise = acc
      where
        stripped = T.strip l

{- | Parse function/value definitions.
Looks for lines matching:
  [pub] fun <name> : <sig> = ...
  [pub] val <name> : <sig> = ...
-}
parseDefs :: [Text] -> [OrganDef]
parseDefs = foldr go []
  where
    go l acc
        | Just rest <- stripFunVal (T.strip l) =
            case parseFunSig rest of
                Just def -> def : acc
                Nothing -> acc
        | otherwise = acc

-- | Strip optional "pub " prefix and "fun "/"val " keyword, return rest.
stripFunVal :: Text -> Maybe Text
stripFunVal t =
    let t' = fromMaybe t (T.stripPrefix "pub " t)
     in case T.stripPrefix "fun " t' of
            Just r -> Just (T.strip r)
            Nothing -> case T.stripPrefix "val " t' of
                Just r -> Just (T.strip r)
                Nothing -> Nothing

{- | Parse "<name> : <sig> = ..." into an OrganDef.
Signature forms:
  (p1 : t1, p2 : t2) -> eff result
  type                                (for vals)
-}
parseFunSig :: Text -> Maybe OrganDef
parseFunSig t = do
    let (nameRaw, afterColon) = T.breakOn " : " t
        name = T.strip nameRaw
    if T.null afterColon
        then Nothing
        else do
            let sig = T.strip (T.drop 3 afterColon) -- drop " : "
            -- Cut at " = " to get just the type signature
                (sigPart, _) = breakAtTopLevel sig
            parseSigToOrgan name sigPart

-- | Break at the first top-level " = " (not inside parens/angle brackets).
breakAtTopLevel :: Text -> (Text, Text)
breakAtTopLevel = go (0 :: Int) (0 :: Int) T.empty
  where
    go !parens !angles acc rest
        | T.null rest = (acc, T.empty)
        | " = " `T.isPrefixOf` rest && parens == 0 && angles == 0 =
            (acc, T.drop 3 rest)
        | otherwise =
            let c = T.head rest
                parens' = case c of
                    '(' -> parens + 1
                    ')' -> max 0 (parens - 1)
                    _ -> parens
                angles' = case c of
                    '<' -> angles + 1
                    '>' -> max 0 (angles - 1)
                    _ -> angles
             in go parens' angles' (T.snoc acc c) (T.tail rest)

-- | Parse a Koka Core type signature into params, effects, return type.
parseSigToOrgan :: Text -> Text -> Maybe OrganDef
parseSigToOrgan name sig
    -- Function type: (params) -> eff result
    | "(" `T.isPrefixOf` sig =
        let (paramsPart, afterParams) = matchParens sig
            params = parseParams paramsPart
         in case T.stripPrefix "->" (T.strip afterParams) of
                Just afterArrow ->
                    let (eff, retTy) = parseEffectAndReturn (T.strip afterArrow)
                     in Just
                            OrganDef
                                { odName = name
                                , odParams = params
                                , odEffects = eff
                                , odReturnType = mapType retTy
                                }
                Nothing ->
                    -- No arrow — treat whole sig as return type (thunk)
                    Just
                        OrganDef
                            { odName = name
                            , odParams = params
                            , odEffects = []
                            , odReturnType = mapType (T.strip afterParams)
                            }
    -- Value type (no parens): just a type
    | otherwise =
        Just
            OrganDef
                { odName = name
                , odParams = []
                , odEffects = []
                , odReturnType = mapType (T.strip sig)
                }

-- | Match balanced parens, return (inside, rest-after-close-paren).
matchParens :: Text -> (Text, Text)
matchParens t
    | Just inner <- T.stripPrefix "(" t = go (1 :: Int) T.empty inner
    | otherwise = (t, T.empty)
  where
    go 0 acc rest = (acc, rest)
    go _ acc rest | T.null rest = (acc, T.empty)
    go n acc rest =
        let c = T.head rest
            n' = case c of
                '(' -> n + 1
                ')' -> n - 1
                _ -> n
         in if n' == 0
                then (acc, T.tail rest)
                else go n' (T.snoc acc c) (T.tail rest)

-- | Parse "p1 : t1, p2 : t2" into [(name, mappedType)].
parseParams :: Text -> [(Text, Text)]
parseParams t
    | T.null (T.strip t) = []
    | otherwise = map parseOneParam (splitParams t)

-- | Split on commas respecting nesting.
splitParams :: Text -> [Text]
splitParams = go (0 :: Int) T.empty
  where
    go _ acc rest | T.null rest = [acc | not (T.null (T.strip acc))]
    go !depth acc rest =
        let c = T.head rest
            rest' = T.tail rest
         in case c of
                '(' -> go (depth + 1) (T.snoc acc c) rest'
                ')' -> go (max 0 (depth - 1)) (T.snoc acc c) rest'
                '<' -> go (depth + 1) (T.snoc acc c) rest'
                '>' -> go (max 0 (depth - 1)) (T.snoc acc c) rest'
                ',' | depth == 0 -> acc : go 0 T.empty rest'
                _ -> go depth (T.snoc acc c) rest'

parseOneParam :: Text -> (Text, Text)
parseOneParam t =
    let stripped = T.strip t
     in case T.breakOn " : " stripped of
            (n, rest)
                | not (T.null rest) -> (T.strip n, mapType (T.strip (T.drop 3 rest)))
                | otherwise -> ("_", mapType stripped)

{- | Parse "eff result" where eff can be:
  total, div, console, <console,div>, <raise|e>, io, etc.
-}
parseEffectAndReturn :: Text -> ([Text], Text)
parseEffectAndReturn t
    -- Effect row in angle brackets: <eff1,eff2|e> result
    | "<" `T.isPrefixOf` t =
        let (effRow, afterEff) = matchAngles t
            effs = parseEffectRow effRow
            retTy = T.strip afterEff
         in (effs, if T.null retTy then "()" else retTy)
    -- Named effect followed by return type
    | otherwise =
        let ws = T.words t
         in case ws of
                [] -> ([], "()")
                [w] ->
                    if isEffectName w
                        then ([mapEffect w], "()")
                        else ([], w)
                (w : rest)
                    | isEffectName w -> ([mapEffect w], T.unwords rest)
                    | otherwise -> ([], t)

-- | Match angle brackets: <...> rest
matchAngles :: Text -> (Text, Text)
matchAngles t
    | Just inner <- T.stripPrefix "<" t = go (1 :: Int) T.empty inner
    | otherwise = (t, T.empty)
  where
    go 0 acc rest = (acc, rest)
    go _ acc rest | T.null rest = (acc, T.empty)
    go n acc rest =
        let c = T.head rest
            n' = case c of
                '<' -> n + 1
                '>' -> n - 1
                _ -> n
         in if n' == 0
                then (acc, T.tail rest)
                else go n' (T.snoc acc c) (T.tail rest)

-- | Parse an effect row: "console,div", "raise|e", "div", etc.
parseEffectRow :: Text -> [Text]
parseEffectRow t =
    let
        -- Split on | first (polymorphic effect row)
        (concrete, _polyVar) = T.breakOn "|" t
        -- Split on commas
        parts = map T.strip (T.splitOn "," concrete)
     in
        map mapEffect (filter (not . T.null) parts)

-- | Is this word a known effect name?
isEffectName :: Text -> Bool
isEffectName w =
    w
        `elem` [ "total"
               , "div"
               , "console"
               , "io"
               , "exn"
               , "raise"
               , "ndet"
               , "alloc"
               , "read"
               , "write"
               , "st"
               , "net"
               , "fsys"
               , "ui"
               , "blocking"
               ]

-- | Map Koka effect names to OrganIR effect names.
mapEffect :: Text -> Text
mapEffect "total" = "pure"
mapEffect "console" = "io"
mapEffect "raise" = "exn"
mapEffect e = e

------------------------------------------------------------------------
-- Type mapping
------------------------------------------------------------------------

mapType :: Text -> Text
mapType t = case T.strip t of
    "int" -> "std.int"
    "float64" -> "std.float"
    "double" -> "std.float"
    "bool" -> "std.bool"
    "string" -> "std.string"
    "()" -> "std.unit"
    _other -> "any"

------------------------------------------------------------------------
-- OrganIR conversion
------------------------------------------------------------------------

moduleToIR :: OrganModule -> IR.OrganIR
moduleToIR m =
    let defs = map defToIR (omDefs m)
        exports = map odName (omDefs m)
    in  IR.OrganIR
            { IR.irMetadata = IR.Metadata IR.LKoka Nothing Nothing "koka-shim-0.1" Nothing
            , IR.irModule = IR.Module (omName m) exports defs [] []
            }

defToIR :: OrganDef -> IR.Definition
defToIR d =
    IR.Definition
        { IR.defName = IR.localName (odName d)
        , IR.defType = defType_
        , IR.defExpr = IR.EVar (IR.Name (odName d) 0)
        , IR.defSort = if null (odParams d) then IR.SVal else IR.SFun
        , IR.defVisibility = IR.Public
        , IR.defArity = length (odParams d)
        }
  where
    effectRow =
        IR.EffectRow
            (map IR.localName (filter (/= "pure") (odEffects d)))
            Nothing
    retTy = IR.tCon (odReturnType d)
    defType_
        | null (odParams d) = retTy
        | otherwise = IR.TFn (map paramToArg (odParams d)) effectRow retTy

paramToArg :: (Text, Text) -> IR.FnArg
paramToArg (_name, ty) = IR.FnArg Nothing (IR.tCon ty)
