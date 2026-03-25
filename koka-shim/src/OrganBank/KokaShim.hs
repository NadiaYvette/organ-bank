module OrganBank.KokaShim (
    extractOrganIR,
) where

import Control.Exception (SomeException, catch)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName)
import System.Process (readProcess, readProcessWithExitCode)

------------------------------------------------------------------------
-- OrganIR types
------------------------------------------------------------------------

data OrganDef = OrganDef
    { odName :: Text
    , odParams :: [(Text, Text)] -- (name, mapped type)
    , odEffects :: [Text]
    , odReturnType :: Text
    , odBody :: Text -- raw body text after " = "
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

-- | Detect koka version by running @koka --version@.
detectCompilerVersion :: IO Text
detectCompilerVersion = do
    (ec, out, _) <- readProcessWithExitCode "koka" ["--version"] ""
    pure $ case ec of
        ExitSuccess | (l : _) <- lines out -> "koka-shim-0.1 (" <> T.strip (T.pack l) <> ")"
        _ -> "koka-shim-0.1"
  `catch` \(_ :: SomeException) -> pure "koka-shim-0.1"

-- | Run koka --showcore on a .kk file and return OrganIR JSON.
extractOrganIR :: FilePath -> IO Text
extractOrganIR kkFile = do
    shimVer <- detectCompilerVersion
    raw <- readProcess "koka" ["--showcore", kkFile] ""
    let coreText = T.pack raw
        modul = parseCoreOutput (T.pack (takeBaseName kkFile)) coreText
    pure (renderOrganIR (moduleToIR shimVer modul))

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
            -- Cut at " = " to get just the type signature and body
                (sigPart, bodyPart) = breakAtTopLevel sig
            parseSigToOrgan name sigPart bodyPart

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
parseSigToOrgan :: Text -> Text -> Text -> Maybe OrganDef
parseSigToOrgan name sig bodyRaw
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
                                , odBody = bodyRaw
                                }
                Nothing ->
                    -- No arrow — treat whole sig as return type (thunk)
                    Just
                        OrganDef
                            { odName = name
                            , odParams = params
                            , odEffects = []
                            , odReturnType = mapType (T.strip afterParams)
                            , odBody = bodyRaw
                            }
    -- Value type (no parens): just a type
    | otherwise =
        Just
            OrganDef
                { odName = name
                , odParams = []
                , odEffects = []
                , odReturnType = mapType (T.strip sig)
                , odBody = bodyRaw
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
-- Koka Core expression parser
------------------------------------------------------------------------

-- | Parse a Koka Core expression, with graceful fallback.
parseKokaExpr :: Text -> IR.Expr
parseKokaExpr raw
    | T.null stripped = IR.eVar "_"
    -- fn(params){ body }
    | Just rest <- T.stripPrefix "fn(" stripped =
        parseFnExpr rest
    -- match(scrut) { branches }
    | Just rest <- T.stripPrefix "match(" stripped =
        parseMatchExpr rest
    -- val x = e; rest  or  val x = e\nrest
    | Just rest <- T.stripPrefix "val " stripped =
        parseValExpr rest
    -- if cond then t else e
    | Just rest <- T.stripPrefix "if " stripped =
        parseIfExpr rest
    -- return(e) → just the inner expression
    | Just rest <- T.stripPrefix "return(" stripped =
        let (inner, _) = matchParens ("(" <> rest)
         in parseKokaExpr inner
    -- String literal
    | "\"" `T.isPrefixOf` stripped =
        case T.stripPrefix "\"" stripped of
            Just afterQuote ->
                let (content, _) = T.breakOn "\"" afterQuote
                 in IR.ELit (IR.LitString content)
            Nothing -> fallbackExpr stripped
    -- Integer/float literal
    | isNumericStart stripped =
        parseLiteral stripped
    -- Function application: f(args)
    | Just (fName, argsText) <- parseFunCall stripped =
        let args = splitArgs argsText
         in IR.EApp (IR.eVar fName) (map parseKokaExpr args)
    -- Binary operation: try to split on top-level operators
    | Just (lhs, op, rhs) <- parseBinOp stripped =
        IR.EApp (IR.eVar op) [parseKokaExpr lhs, parseKokaExpr rhs]
    -- Bare identifier (variable)
    | isIdentifier stripped =
        IR.eVar stripped
    -- Fallback: wrap as opaque Koka Core text
    | otherwise = fallbackExpr stripped
  where
    stripped = T.strip raw

-- | Fallback: wrap unparsed text as application of "koka-core" to the raw text.
fallbackExpr :: Text -> IR.Expr
fallbackExpr t = IR.eApp (IR.eVar "koka-core") [IR.eString t]

-- | Parse fn(params){ body }
parseFnExpr :: Text -> IR.Expr
parseFnExpr rest =
    let -- rest starts after "fn(", so re-add "(" for matchParens
        (paramText, afterParams) = matchParens ("(" <> rest)
        params = parseLamParams paramText
        bodyText = extractBraceBody (T.strip afterParams)
     in IR.ELam params (parseKokaExpr bodyText)

-- | Parse lambda parameters: "x: int, y: string" → [LamParam]
parseLamParams :: Text -> [IR.LamParam]
parseLamParams t
    | T.null (T.strip t) = []
    | otherwise = map parseOneLamParam (splitParams t)

parseOneLamParam :: Text -> IR.LamParam
parseOneLamParam t =
    let stripped = T.strip t
     in case T.breakOn ":" stripped of
            (n, rest)
                | not (T.null rest) ->
                    let ty = T.strip (T.drop 1 rest)
                     in IR.LamParam (IR.Name (T.strip n) 0) (Just (IR.tCon (mapType ty)))
                | otherwise ->
                    IR.LamParam (IR.Name stripped 0) Nothing

-- | Parse match(scrut) { pat -> body; ... }
parseMatchExpr :: Text -> IR.Expr
parseMatchExpr rest =
    let (scrutText, afterScrut) = matchParens ("(" <> rest)
        scrut = parseKokaExpr scrutText
        branchText = extractBraceBody (T.strip afterScrut)
        branches = parseBranches branchText
     in IR.ECase scrut branches

-- | Parse branches from inside { ... }, separated by newlines or semicolons.
parseBranches :: Text -> [IR.Branch]
parseBranches t =
    let rawBranches = splitBranches t
     in mapMaybe parseOneBranch rawBranches
  where
    mapMaybe _ [] = []
    mapMaybe f (x : xs) = case f x of
        Just b -> b : mapMaybe f xs
        Nothing -> mapMaybe f xs

-- | Split branch text on top-level newlines or semicolons containing "->".
splitBranches :: Text -> [Text]
splitBranches t =
    let -- First try splitting on newlines
        lns = map T.strip (T.lines t)
        -- Filter out empty lines
        nonEmpty = filter (not . T.null) lns
     in if any ("->" `T.isInfixOf`) nonEmpty
            then nonEmpty
            else -- Try semicolons
                map T.strip (splitTopLevel ';' t)

-- | Parse a single branch: "Pat -> body"
parseOneBranch :: Text -> Maybe IR.Branch
parseOneBranch t =
    let (patText, bodyText) = breakAtArrow t
     in if T.null bodyText
            then Nothing
            else
                Just
                    IR.Branch
                        { IR.brPattern = parsePattern (T.strip patText)
                        , IR.brBody = parseKokaExpr (T.strip bodyText)
                        }

-- | Break at top-level " -> " (not inside parens/braces).
breakAtArrow :: Text -> (Text, Text)
breakAtArrow = go (0 :: Int) T.empty
  where
    go !depth acc rest
        | T.null rest = (acc, T.empty)
        | " -> " `T.isPrefixOf` rest && depth == 0 =
            (acc, T.drop 4 rest)
        | "->" `T.isPrefixOf` rest && depth == 0 && T.null acc =
            -- Handle "->body" at start (unlikely but safe)
            (acc, T.drop 2 rest)
        | otherwise =
            let c = T.head rest
                depth' = case c of
                    '(' -> depth + 1
                    ')' -> max 0 (depth - 1)
                    '{' -> depth + 1
                    '}' -> max 0 (depth - 1)
                    _ -> depth
             in go depth' (T.snoc acc c) (T.tail rest)

-- | Parse a pattern: constructor, literal, variable, or wildcard.
parsePattern :: Text -> IR.Pat
parsePattern t
    | T.null t = IR.PatWild
    | t == "_" = IR.PatWild
    | t == "True" || t == "true" = IR.PatCon (IR.localName "true") []
    | t == "False" || t == "false" = IR.PatCon (IR.localName "false") []
    -- Constructor with args: Con(x, y)
    | Just (cName, argsText) <- parseFunCall t =
        let binders = map (\a -> IR.PatBinder (IR.Name (T.strip a) 0) Nothing)
                          (splitArgs argsText)
         in IR.PatCon (IR.localName cName) binders
    -- Integer literal pattern
    | isNumericStart t =
        case parseLiteral t of
            IR.ELit lit -> IR.PatLit lit
            _ -> IR.PatVar (IR.Name t 0) Nothing
    -- String literal pattern
    | "\"" `T.isPrefixOf` t =
        case T.stripPrefix "\"" t of
            Just afterQuote ->
                let (content, _) = T.breakOn "\"" afterQuote
                 in IR.PatLit (IR.LitString content)
            Nothing -> IR.PatVar (IR.Name t 0) Nothing
    -- Variable pattern
    | isIdentifier t = IR.PatVar (IR.Name t 0) Nothing
    -- Fallback
    | otherwise = IR.PatVar (IR.Name t 0) Nothing

-- | Parse "val x = e; rest" or "val x = e\nrest"
parseValExpr :: Text -> IR.Expr
parseValExpr rest =
    let (binding, after) = breakValBinding rest
        (vName, valExpr) = parseValBinding binding
     in if T.null (T.strip after)
            then IR.ELet [IR.LetBind (IR.Name vName 0) Nothing (parseKokaExpr valExpr)]
                         (IR.eVar vName)
            else IR.ELet [IR.LetBind (IR.Name vName 0) Nothing (parseKokaExpr valExpr)]
                         (parseKokaExpr after)

-- | Split "x = expr; rest" or "x = expr\nrest" into (binding, rest).
breakValBinding :: Text -> (Text, Text)
breakValBinding t =
    -- Look for semicolon or newline after the binding
    let (beforeEq, afterEq) = breakAtTopLevel t
     in case T.breakOn ";" afterEq of
            (expr, rest)
                | not (T.null rest) -> (beforeEq <> " = " <> expr, T.drop 1 rest)
                | otherwise ->
                    -- Try newline split
                    case T.breakOn "\n" afterEq of
                        (expr', rest')
                            | not (T.null rest') -> (beforeEq <> " = " <> expr', T.drop 1 rest')
                            | otherwise -> (t, T.empty)

-- | Parse "x = expr" or "x : type = expr" into (name, exprText).
parseValBinding :: Text -> (Text, Text)
parseValBinding t =
    let (lhs, rhs) = breakAtTopLevel t
     in case T.breakOn " : " lhs of
            (n, _rest)
                | not (T.null _rest) -> (T.strip n, rhs)
                | otherwise -> (T.strip lhs, rhs)

-- | Parse "if cond then t else e"
parseIfExpr :: Text -> IR.Expr
parseIfExpr rest =
    case breakAtKeyword "then" rest of
        Just (condText, afterThen) ->
            case breakAtKeyword "else" afterThen of
                Just (thenText, elseText) ->
                    IR.ECase
                        (parseKokaExpr condText)
                        [ IR.Branch (IR.PatCon (IR.localName "true") []) (parseKokaExpr thenText)
                        , IR.Branch (IR.PatCon (IR.localName "false") []) (parseKokaExpr elseText)
                        ]
                Nothing ->
                    -- No else branch
                    IR.ECase
                        (parseKokaExpr condText)
                        [ IR.Branch (IR.PatCon (IR.localName "true") []) (parseKokaExpr afterThen)
                        , IR.Branch (IR.PatCon (IR.localName "false") []) (IR.eVar "_")
                        ]
        Nothing -> fallbackExpr ("if " <> rest)

-- | Break at a top-level keyword (surrounded by spaces, not inside nesting).
breakAtKeyword :: Text -> Text -> Maybe (Text, Text)
breakAtKeyword kw = go (0 :: Int) T.empty
  where
    kwPat = " " <> kw <> " "
    kwLen = T.length kwPat
    go !depth acc rest
        | T.null rest = Nothing
        | kwPat `T.isPrefixOf` rest && depth == 0 =
            Just (acc, T.drop kwLen rest)
        | otherwise =
            let c = T.head rest
                depth' = case c of
                    '(' -> depth + 1
                    ')' -> max 0 (depth - 1)
                    '{' -> depth + 1
                    '}' -> max 0 (depth - 1)
                    _ -> depth
             in go depth' (T.snoc acc c) (T.tail rest)

-- | Extract body from { ... }, stripping outer braces.
extractBraceBody :: Text -> Text
extractBraceBody t
    | Just inner <- T.stripPrefix "{" t =
        -- Find matching close brace
        let (body, _) = matchBraces inner
         in T.strip body
    | otherwise = t

-- | Match balanced braces, return (inside, rest-after-close-brace).
matchBraces :: Text -> (Text, Text)
matchBraces = go (1 :: Int) T.empty
  where
    go 0 acc rest = (acc, rest)
    go _ acc rest | T.null rest = (acc, T.empty)
    go n acc rest =
        let c = T.head rest
            n' = case c of
                '{' -> n + 1
                '}' -> n - 1
                _ -> n
         in if n' == 0
                then (acc, T.tail rest)
                else go n' (T.snoc acc c) (T.tail rest)

-- | Try to parse "f(args)" → Just (f, argsText).
parseFunCall :: Text -> Maybe (Text, Text)
parseFunCall t =
    let (ident, rest) = T.span isIdentChar t
     in if T.null ident || T.null rest
            then Nothing
            else case T.uncons rest of
                Just ('(', _) ->
                    let (args, _) = matchParens rest
                     in Just (ident, args)
                _ -> Nothing

-- | Split arguments at top-level commas (reuses splitParams).
splitArgs :: Text -> [Text]
splitArgs = splitParams

-- | Try to parse a binary operation: a op b.
-- Checks for common operators at the top level.
parseBinOp :: Text -> Maybe (Text, Text, Text)
parseBinOp t = go operators
  where
    -- Ordered longest-first to avoid prefix conflicts
    operators :: [Text]
    operators = ["<=", ">=", "!=", "==", "&&", "||", "+", "-", "*", "/", "%", "<", ">"]
    go [] = Nothing
    go (op : ops) =
        case breakAtOp op t of
            Just (lhs, rhs)
                | not (T.null (T.strip lhs)) && not (T.null (T.strip rhs)) ->
                    Just (T.strip lhs, op, T.strip rhs)
            _ -> go ops

-- | Break at a top-level binary operator (space-surrounded, not inside nesting).
breakAtOp :: Text -> Text -> Maybe (Text, Text)
breakAtOp op = go (0 :: Int) T.empty
  where
    pat = " " <> op <> " "
    patLen = T.length pat
    go !depth acc rest
        | T.null rest = Nothing
        | pat `T.isPrefixOf` rest && depth == 0 =
            Just (acc, T.drop patLen rest)
        | otherwise =
            let c = T.head rest
                depth' = case c of
                    '(' -> depth + 1
                    ')' -> max 0 (depth - 1)
                    '{' -> depth + 1
                    '}' -> max 0 (depth - 1)
                    _ -> depth
             in go depth' (T.snoc acc c) (T.tail rest)

-- | Check if text starts with a digit or negative sign followed by digit.
isNumericStart :: Text -> Bool
isNumericStart t = case T.uncons t of
    Just (c, _) | c >= '0' && c <= '9' -> True
    Just ('-', rest) -> case T.uncons rest of
        Just (c, _) | c >= '0' && c <= '9' -> True
        _ -> False
    _ -> False

-- | Parse an integer or float literal.
parseLiteral :: Text -> IR.Expr
parseLiteral t =
    let numText = T.takeWhile (\c -> c >= '0' && c <= '9' || c == '.' || c == '-' || c == 'e' || c == 'E') t
     in if "." `T.isInfixOf` numText || "e" `T.isInfixOf` numText || "E" `T.isInfixOf` numText
            then case reads (T.unpack numText) :: [(Double, String)] of
                [(d, "")] -> IR.ELit (IR.LitFloat d)
                _ -> fallbackExpr t
            else case reads (T.unpack numText) :: [(Integer, String)] of
                [(n, "")] -> IR.ELit (IR.LitInt n)
                _ -> fallbackExpr t

-- | Is the character valid in an identifier?
isIdentChar :: Char -> Bool
isIdentChar c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
    || c >= '0' && c <= '9' || c == '_' || c == '-' || c == '/' || c == '.'

-- | Is the text a valid identifier (non-empty, starts with letter or _)?
isIdentifier :: Text -> Bool
isIdentifier t = case T.uncons t of
    Just (c, rest) -> (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_')
        && T.all isIdentChar rest
    Nothing -> False

-- | Split text at top-level occurrences of a separator character.
splitTopLevel :: Char -> Text -> [Text]
splitTopLevel sep = go (0 :: Int) T.empty
  where
    go _ acc rest | T.null rest = [acc | not (T.null (T.strip acc))]
    go !depth acc rest =
        let c = T.head rest
            rest' = T.tail rest
         in case c of
                _ | c == sep && depth == 0 -> acc : go 0 T.empty rest'
                '(' -> go (depth + 1) (T.snoc acc c) rest'
                ')' -> go (max 0 (depth - 1)) (T.snoc acc c) rest'
                '{' -> go (depth + 1) (T.snoc acc c) rest'
                '}' -> go (max 0 (depth - 1)) (T.snoc acc c) rest'
                _ -> go depth (T.snoc acc c) rest'

------------------------------------------------------------------------
-- OrganIR conversion
------------------------------------------------------------------------

moduleToIR :: Text -> OrganModule -> IR.OrganIR
moduleToIR shimVer m =
    let defs = map defToIR (omDefs m)
        exports = map odName (omDefs m)
    in  IR.OrganIR
            { IR.irMetadata = IR.Metadata IR.LKoka Nothing Nothing shimVer Nothing
            , IR.irModule = IR.Module (omName m) exports [] defs [] []
            }

defToIR :: OrganDef -> IR.Definition
defToIR d =
    IR.Definition
        { IR.defName = IR.localName (odName d)
        , IR.defType = defType_
        , IR.defExpr = parseKokaExpr (odBody d)
        , IR.defSort = if null (odParams d) then IR.SVal else IR.SFun
        , IR.defVisibility = IR.Public
        , IR.defArity = length (odParams d)
        }
  where
    effectRow =
        IR.EffectRow
            (map (\e -> IR.qualName "koka" e) (filter (/= "pure") (odEffects d)))
            Nothing
    retTy = IR.tCon (odReturnType d)
    defType_
        | null (odParams d) = retTy
        | otherwise = IR.TFn (map paramToArg (odParams d)) effectRow retTy

paramToArg :: (Text, Text) -> IR.FnArg
paramToArg (_name, ty) = IR.FnArg Nothing (IR.tCon ty)
