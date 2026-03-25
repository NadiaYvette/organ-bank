{- | Mercury HLDS Extraction Shim

Runs mmc --dump-hlds 50 to get the HLDS dump after mode/determinism
analysis, then parses the text output and emits OrganIR JSON.

Based on Frankenstein's MercuryBridge/HldsParse.hs.
-}
module OrganBank.MmcShim (
    extractOrganIR,
    parseHldsGoal,
) where

import Control.Exception (SomeException, catch)
import Control.Exception qualified
import Data.Char (isAlphaNum, isDigit, isSpace, isUpper)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

-- | Detect mmc version by running @mmc --version@.
detectCompilerVersion :: IO Text
detectCompilerVersion = do
    (ec, out, _) <- readProcessWithExitCode "mmc" ["--version"] ""
    pure $ case ec of
        ExitSuccess | (l : _) <- lines out -> "mmc-shim-0.1 (" <> T.strip (T.pack l) <> ")"
        _ -> "mmc-shim-0.1"
  `catch` \(_ :: SomeException) -> pure "mmc-shim-0.1"

-- | Extract Mercury HLDS from a source file and emit OrganIR JSON.
extractOrganIR :: FilePath -> IO (Either Text Text)
extractOrganIR inputPath = do
    shimVer <- detectCompilerVersion
    -- Mercury compiler dumps HLDS to stderr with --dump-hlds
    (ec, _stdout, stderrOut) <-
        readProcessWithExitCode "mmc" ["--dump-hlds", "50", inputPath] ""
    -- mmc may return non-zero even on success with --dump-hlds
    let hldsText = stderrOut
    -- Also check for .hlds file
    let hldsFile = takeBaseName inputPath ++ ".hlds_dump"
    hldsFromFile <- tryReadFile hldsFile
    let dump = fromMaybe hldsText hldsFromFile
    case ec of
        ExitSuccess -> pure ()
        ExitFailure _ ->
            hPutStrLn stderr "mmc exited with error (dump may still be usable)"
    if null dump
        then pure $ Left "No HLDS output from mmc"
        else do
            let modName = takeBaseName inputPath
                preds = parseHldsDump dump
                defs = zipWith predToIR [1 ..] preds
                exports = map (T.pack . predName) preds
                ir =
                    renderOrganIR $
                        IR.OrganIR
                            { IR.irMetadata = IR.Metadata IR.LMercury Nothing Nothing shimVer Nothing
                            , IR.irModule = IR.Module (T.pack modName) exports [] defs [] []
                            }
            pure $ Right ir

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile fp = do
    result <- Control.Exception.try @IOError (readFile fp >>= \c -> length c `seq` pure c)
    pure $ either (const Nothing) Just result

-- | A parsed Mercury predicate from HLDS dump.
data HldsPred = HldsPred
    { predName :: String
    , predArity :: Int
    , predDet :: String -- "det", "semidet", "multi", "nondet", "erroneous", "failure"
    , predModes :: [String] -- mode declarations
    , predClauseText :: String -- raw clause body text from HLDS
    }
    deriving (Show)

-- | Parse the HLDS dump text to extract predicate declarations.
parseHldsDump :: String -> [HldsPred]
parseHldsDump dump =
    let ls = lines dump
     in extractPreds ls

extractPreds :: [String] -> [HldsPred]
extractPreds [] = []
extractPreds (l : ls)
    | ":- pred " `isPrefixOf` stripped =
        let predDecl = drop 8 stripped -- drop ":- pred "
            (name', rest) = break (== '(') predDecl
            arity = countArgs rest
            (det, modes, clauseText, remaining) = findDetAndModes ls
         in HldsPred (trim name') arity det modes clauseText : extractPreds remaining
    | otherwise = extractPreds ls
  where
    stripped = dropWhile isSpace l

countArgs :: String -> Int
countArgs s =
    let inner = takeWhile (/= ')') (drop 1 s) -- drop the '('
     in if null inner
            then 0
            else length (filter (== ',') inner) + 1

findDetAndModes :: [String] -> (String, [String], String, [String])
findDetAndModes ls =
    let (block, rest) = span (\l' -> not (":- pred " `isPrefixOf` dropWhile isSpace l') && not (null l')) ls
        det = findDet block
        modes = findModes block
        clauseText = findClauseText block
     in (det, modes, clauseText, rest)

-- | Extract clause body text from a predicate block in the HLDS dump.
findClauseText :: [String] -> String
findClauseText ls =
    let clauseLines = [trim l | l <- ls
                       , not (":- mode " `isPrefixOf` dropWhile isSpace l)
                       , not (":- pred " `isPrefixOf` dropWhile isSpace l)
                       , not ("is " `isPrefixOf` dropWhile isSpace l && any (`isInfixOf'` l) ["is det", "is semidet", "is multi", "is nondet", "is erroneous", "is failure"])
                       , not (null (trim l))
                       ]
     in unlines clauseLines

findDet :: [String] -> String
findDet [] = "det"
findDet (l : ls)
    | "is det" `isInfixOf'` l = "det"
    | "is semidet" `isInfixOf'` l = "semidet"
    | "is multi" `isInfixOf'` l = "multi"
    | "is nondet" `isInfixOf'` l = "nondet"
    | "is erroneous" `isInfixOf'` l = "erroneous"
    | "is failure" `isInfixOf'` l = "failure"
    | otherwise = findDet ls

isInfixOf' :: String -> String -> Bool
isInfixOf' needle haystack = any (isPrefixOf needle) (tails' haystack)
  where
    tails' [] = [[]]
    tails' s@(_ : xs) = s : tails' xs

findModes :: [String] -> [String]
findModes ls =
    [trim l | l <- ls, ":- mode " `isPrefixOf` dropWhile isSpace l]

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Convert Mercury determinism to an OrganIR effect row.
detToEffectRow :: String -> IR.EffectRow
detToEffectRow "det" = IR.pureEffect
detToEffectRow "semidet" = IR.EffectRow [IR.qualName "std" "exn"] Nothing
detToEffectRow "multi" = IR.EffectRow [IR.qualName "std" "choice"] Nothing
detToEffectRow "nondet" = IR.EffectRow [IR.qualName "std" "exn", IR.qualName "std" "choice"] Nothing
detToEffectRow "erroneous" = IR.EffectRow [IR.qualName "std" "exn"] Nothing
detToEffectRow "failure" = IR.EffectRow [IR.qualName "std" "exn"] Nothing
detToEffectRow _ = IR.pureEffect

-- | Determine multiplicity from Mercury modes.
-- Mercury "unique" and "clobbered" modes indicate affine usage.
modeMultiplicity :: [String] -> IR.Multiplicity
modeMultiplicity modes
    | any (\m -> "unique" `isInfixOf'` m || "clobbered" `isInfixOf'` m) modes = IR.Affine
    | otherwise = IR.Many

-- | Convert a parsed HLDS predicate to an OrganIR definition.
predToIR :: Int -> HldsPred -> IR.Definition
predToIR uid pred' =
    let mult = modeMultiplicity (predModes pred')
        clauseTxt = T.pack (predClauseText pred')
        bodyExpr
            | T.null (T.strip clauseTxt) =
                IR.EApp (IR.eVar "hlds_clause") [IR.eString "<empty>"]
            | otherwise = parseHldsGoal clauseTxt
     in IR.Definition
        { IR.defName = IR.QName "" (IR.Name (T.pack (predName pred')) uid)
        , IR.defType =
            IR.TFn
                (replicate (predArity pred') (IR.FnArg (Just mult) IR.TAny))
                (detToEffectRow (predDet pred'))
                IR.TAny
        , IR.defExpr = bodyExpr
        , IR.defSort = IR.SFun
        , IR.defVisibility = IR.Public
        , IR.defArity = predArity pred'
        }

-- ---------------------------------------------------------------------------
-- Mercury HLDS goal parser
-- ---------------------------------------------------------------------------

-- | Parse a Mercury HLDS goal structure into OrganIR Expr.
-- Best-effort: falls back to @eApp (eVar "hlds") [eString text]@.
parseHldsGoal :: Text -> IR.Expr
parseHldsGoal txt =
    let stripped = T.strip txt
     in parseGoal (T.unpack stripped)

-- | Fallback: wrap unparsed text.
hldsFallback :: String -> IR.Expr
hldsFallback s = IR.EApp (IR.eVar "hlds") [IR.eString (T.pack s)]

-- | Parse a goal string.
parseGoal :: String -> IR.Expr
parseGoal s =
    let s' = trimHlds s
     in case s' of
            -- Empty
            [] -> hldsFallback ""
            -- Parenthesized if-then-else: ( cond -> then ; else )
            ('(' : rest) -> parseParenGoal rest
            -- Try conjunction, disjunction, unification, call
            _ -> parseConjunction s'

-- | Parse a parenthesized goal: ( cond -> then ; else ) or ( disj1 ; disj2 )
parseParenGoal :: String -> IR.Expr
parseParenGoal s =
    -- Find the matching close paren and extract inner content
    let inner = takeMatchingParen s
        inner' = trimHlds inner
     in case splitOnArrow inner' of
            Just (cond, thenElse) ->
                -- if-then-else: cond -> then ; else
                case splitOnTopSemicolon thenElse of
                    Just (thenPart, elsePart) ->
                        let condExpr = parseGoal (trimHlds cond)
                            thenExpr = parseGoal (trimHlds thenPart)
                            elseExpr = parseGoal (trimHlds elsePart)
                         in IR.ECase condExpr
                                [ IR.Branch (IR.PatCon (IR.localName "true") []) thenExpr
                                , IR.Branch (IR.PatCon (IR.localName "false") []) elseExpr
                                ]
                    Nothing ->
                        let condExpr = parseGoal (trimHlds cond)
                            thenExpr = parseGoal (trimHlds thenElse)
                         in IR.ECase condExpr
                                [ IR.Branch (IR.PatCon (IR.localName "true") []) thenExpr
                                ]
            Nothing ->
                -- Maybe disjunction: goal1 ; goal2
                case splitOnTopSemicolon inner' of
                    Just (left, right) ->
                        let leftExpr = parseGoal (trimHlds left)
                            rightExpr = parseGoal (trimHlds right)
                         in IR.ECase (IR.eVar "_disj")
                                [ IR.Branch (IR.PatCon (IR.localName "left") []) leftExpr
                                , IR.Branch (IR.PatCon (IR.localName "right") []) rightExpr
                                ]
                    Nothing -> parseGoal inner'

-- | Parse a conjunction: goal1, goal2, goal3
-- Splits on top-level commas and nests as ELet bindings.
parseConjunction :: String -> IR.Expr
parseConjunction s =
    let parts = splitOnTopComma s
     in case parts of
            [] -> hldsFallback s
            [single] -> parseSingleGoal (trimHlds single)
            _ -> conjunctionToLet parts

-- | Convert a list of conjunct strings to nested ELet bindings.
conjunctionToLet :: [String] -> IR.Expr
conjunctionToLet [] = hldsFallback ""
conjunctionToLet [single] = parseSingleGoal (trimHlds single)
conjunctionToLet (g : gs) =
    let goalExpr = parseSingleGoal (trimHlds g)
     in case parseAsUnification (trimHlds g) of
            Just (var, rhs) ->
                -- X = expr becomes let X = expr in rest
                IR.ELet
                    [IR.LetBind (IR.Name (T.pack var) 0) Nothing rhs]
                    (conjunctionToLet gs)
            Nothing ->
                -- Non-unification goal: bind to _gN
                let bindName = "_g" ++ show (length gs)
                 in IR.ELet
                        [IR.LetBind (IR.Name (T.pack bindName) 0) Nothing goalExpr]
                        (conjunctionToLet gs)

-- | Parse a single atomic goal (not conjunction/disjunction).
parseSingleGoal :: String -> IR.Expr
parseSingleGoal s =
    let s' = trimHlds s
     in case s' of
            [] -> hldsFallback ""
            ('(' : _) -> parseParenGoal (drop 1 s')
            _ | Just (var, rhs) <- parseAsUnification s' -> rhs
              | Just (pred', args) <- parseAsCall s' ->
                    IR.EApp (IR.eVar (T.pack pred')) (map (IR.eVar . T.pack) args)
              | Just n <- readInt s' -> IR.eInt n
              | otherwise -> hldsFallback s'

-- | Try to parse "X = expr" as a unification.
parseAsUnification :: String -> Maybe (String, IR.Expr)
parseAsUnification s =
    case splitOnTopEquals s of
        Just (lhs, rhs) ->
            let lhs' = trimHlds lhs
                rhs' = trimHlds rhs
             in if isVarName lhs'
                    then Just (lhs', parseRhsExpr rhs')
                    else Nothing
        Nothing -> Nothing

-- | Parse the RHS of a unification.
parseRhsExpr :: String -> IR.Expr
parseRhsExpr s
    | Just n <- readInt s = IR.eInt n
    | Just inner <- stripQuotesHlds s = IR.eString (T.pack inner)
    | Just (f, args) <- parseAsCall s =
        IR.EApp (IR.eVar (T.pack f)) (map (IR.eVar . T.pack) args)
    | isVarName s = IR.eVar (T.pack s)
    | otherwise =
        -- Try to parse infix expression like "X - 1" or "X * W"
        case parseInfix s of
            Just expr -> expr
            Nothing -> hldsFallback s

-- | Parse infix expression: A op B
parseInfix :: String -> Maybe IR.Expr
parseInfix s =
    let tokens = words s
     in case tokens of
            [a, op, b] | op `elem` ["+", "-", "*", "/", "//", "mod", "rem", "<", ">", "=<", ">=", "=:=", "=\\="] ->
                Just $ IR.EApp (IR.eVar (T.pack op)) [tokenToExpr a, tokenToExpr b]
            _ -> Nothing

-- | Convert a single token to an expression.
tokenToExpr :: String -> IR.Expr
tokenToExpr s
    | Just n <- readInt s = IR.eInt n
    | otherwise = IR.eVar (T.pack s)

-- | Try to parse "pred(arg1, arg2, ...)" as a call.
parseAsCall :: String -> Maybe (String, [String])
parseAsCall s =
    let (name', rest) = span (\c -> isAlphaNum c || c == '_' || c == '.' || c == ':') s
     in case rest of
            ('(' : args) ->
                let argStr = takeMatchingParen args
                    argList = splitArgs argStr
                 in if null name' then Nothing else Just (name', map trimHlds argList)
            _ -> Nothing

-- | Split arguments on commas, respecting nesting.
splitArgs :: String -> [String]
splitArgs = splitOnTopChar ','

-- | Check if a string is a Mercury variable name (starts with uppercase or _).
isVarName :: String -> Bool
isVarName [] = False
isVarName (c : _) = isUpper c || c == '_'

-- | Split on top-level " -> " (not inside parens).
splitOnArrow :: String -> Maybe (String, String)
splitOnArrow = splitOnTopStr " -> "

-- | Split on top-level " ; " or ";\n" (not inside parens).
splitOnTopSemicolon :: String -> Maybe (String, String)
splitOnTopSemicolon s = splitOnTopStr ";" s

-- | Split on top-level " = " (not inside parens).
splitOnTopEquals :: String -> Maybe (String, String)
splitOnTopEquals = splitOnTopStr " = "

-- | Split on a top-level delimiter string, not inside parentheses.
splitOnTopStr :: String -> String -> Maybe (String, String)
splitOnTopStr delim = go 0 []
  where
    n = length delim
    go :: Int -> String -> String -> Maybe (String, String)
    go _ _ [] = Nothing
    go 0 acc s
        | take n s == delim = Just (reverse acc, drop n s)
    go depth acc ('(' : rest) = go (depth + 1) ('(' : acc) rest
    go depth acc (')' : rest) = go (max 0 (depth - 1)) (')' : acc) rest
    go depth acc (c : rest) = go depth (c : acc) rest

-- | Split on top-level commas, not inside parentheses.
splitOnTopComma :: String -> [String]
splitOnTopComma = splitOnTopChar ','

-- | Split on a top-level character, respecting parentheses.
splitOnTopChar :: Char -> String -> [String]
splitOnTopChar sep = go 0 [] []
  where
    go :: Int -> String -> [String] -> String -> [String]
    go _ cur acc [] = reverse (reverse cur : acc)
    go 0 cur acc (c : rest)
        | c == sep = go 0 [] (reverse cur : acc) rest
    go depth cur acc ('(' : rest) = go (depth + 1) ('(' : cur) acc rest
    go depth cur acc (')' : rest) = go (max 0 (depth - 1)) (')' : cur) acc rest
    go depth cur acc (c : rest) = go depth (c : cur) acc rest

-- | Take content inside matching parentheses.
takeMatchingParen :: String -> String
takeMatchingParen = go (0 :: Int) []
  where
    go _ acc [] = reverse acc
    go 0 acc (')' : _) = reverse acc
    go n acc ('(' : rest) = go (n + 1) ('(' : acc) rest
    go n acc (')' : rest) = go (n - 1) (')' : acc) rest
    go n acc (c : rest) = go n (c : acc) rest

-- | Trim whitespace.
trimHlds :: String -> String
trimHlds = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Try to read an integer.
readInt :: String -> Maybe Integer
readInt s = case s of
    ('-' : rest) | not (null rest), all isDigit rest -> Just (read s)
    _ | not (null s), all isDigit s -> Just (read s)
    _ -> Nothing

-- | Strip surrounding quotes.
stripQuotesHlds :: String -> Maybe String
stripQuotesHlds ('"' : rest) = case reverse rest of
    ('"' : inner) -> Just (reverse inner)
    _ -> Nothing
stripQuotesHlds _ = Nothing
