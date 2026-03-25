{- | Idris 2 Case Tree Extraction Shim

Runs idris2 --dumpcases to get post-erasure case trees, then
parses the text output and emits OrganIR JSON.

Idris 2's --dumpcases output format:
  Main.factorial =
    \{arg:0} =>
      case arg:0 of
        0 => 1
        _ => (* arg:0 (Main.factorial (- arg:0 1)))

We parse this into definitions with case-tree bodies.
-}
module OrganBank.Idris2Shim (
    extractOrganIR,
    parseCaseTree,
) where

import Control.Exception (SomeException, catch)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName)
import System.Process (readProcessWithExitCode)

-- | Detect idris2 version by running @idris2 --version@.
detectCompilerVersion :: IO Text
detectCompilerVersion = do
    (ec, out, _) <- readProcessWithExitCode "idris2" ["--version"] ""
    pure $ case ec of
        ExitSuccess | (l : _) <- lines out -> "idris2-shim-0.1 (" <> T.strip (T.pack l) <> ")"
        _ -> "idris2-shim-0.1"
  `catch` \(_ :: SomeException) -> pure "idris2-shim-0.1"

-- | Extract Idris 2 case trees from a source file and emit OrganIR JSON.
extractOrganIR :: FilePath -> IO (Either Text Text)
extractOrganIR inputPath = do
    shimVer <- detectCompilerVersion
    let casesFile = "build/exec/" ++ takeBaseName inputPath ++ ".cases"
    -- First compile to get the case trees
    (ec, _stdout, stderrOut) <-
        readProcessWithExitCode
            "idris2"
            ["--dumpcases", casesFile, inputPath]
            ""
    case ec of
        ExitSuccess -> do
            dump <- readFile casesFile
            let modName = takeBaseName inputPath
                defs = parseCasesDump dump
            pure $ Right $ emitOrganIR shimVer modName defs
        ExitFailure _ -> do
            -- Try alternative: --dump-anf or --dump-lifted
            (ec2, _stdout2, _stderr2) <-
                readProcessWithExitCode
                    "idris2"
                    ["--dumpcases", casesFile, "--check", inputPath]
                    ""
            case ec2 of
                ExitSuccess -> do
                    dump <- readFile casesFile
                    let modName = takeBaseName inputPath
                        defs = parseCasesDump dump
                    pure $ Right $ emitOrganIR shimVer modName defs
                ExitFailure _ ->
                    pure $ Left $ T.pack $ "idris2 failed: " ++ stderrOut

-- | A parsed Idris 2 definition from --dumpcases output.
data Idris2Def = Idris2Def
    { idris2DefName :: String -- fully qualified name
    , idris2DefArity :: Int -- number of lambda parameters
    , idris2DefHasCase :: Bool -- whether body contains case expressions
    , idris2DefBodyText :: String -- raw case tree text
    }
    deriving (Show)

{- | Parse --dumpcases output into definitions.
Format:
  Qualified.Name =
    \{arg:0} => \{arg:1} =>
      <body>

  Next.Name =
    ...
-}
parseCasesDump :: String -> [Idris2Def]
parseCasesDump dump =
    let ls = lines dump
     in extractDefs ls

extractDefs :: [String] -> [Idris2Def]
extractDefs [] = []
extractDefs (l : ls)
    | " =" `isSuffixOf'` stripped && not (null defn) =
        let (body, rest) = spanBody ls
            arity = countLambdas body
            hasCase = any (("case " `isPrefixOf`) . dropWhile isSpace) body
            bodyText = unlines body
         in Idris2Def defn arity hasCase bodyText : extractDefs rest
    | otherwise = extractDefs ls
  where
    stripped = dropWhile isSpace l
    defn = takeWhile (\c -> isAlphaNum c || c == '.' || c == '_') stripped

isSuffixOf' :: String -> String -> Bool
isSuffixOf' suffix s =
    let n = length suffix
        m = length s
     in m >= n && drop (m - n) s == suffix

spanBody :: [String] -> ([String], [String])
spanBody = span (\l -> case l of { [] -> True; (c:_) -> isSpace c })

countLambdas :: [String] -> Int
countLambdas ls =
    length [() | l <- ls, "\\{" `isPrefixOf` dropWhile isSpace l]

-- | Convert a parsed Idris2Def to an OrganIR Definition.
defToIR :: Int -> Idris2Def -> IR.Definition
defToIR uid def' =
    let n = T.pack (idris2DefName def')
        arity = idris2DefArity def'
        qname = IR.QName "" (IR.Name n (fromIntegral uid))
        ty
            | arity == 0 = IR.TAny
            | otherwise = IR.TFn (replicate arity (IR.FnArg (Just IR.Many) IR.TAny)) IR.pureEffect IR.TAny
        bodyText = T.pack (idris2DefBodyText def')
        bodyExpr
            | T.null (T.strip bodyText) =
                IR.EApp (IR.eVar "case_tree") [IR.eString "<empty>"]
            | otherwise = parseCaseTree bodyText
     in IR.Definition
            { IR.defName = qname
            , IR.defType = ty
            , IR.defExpr = bodyExpr
            , IR.defSort = IR.SFun
            , IR.defVisibility = IR.Public
            , IR.defArity = arity
            }

-- | Emit OrganIR JSON from parsed definitions.
emitOrganIR :: Text -> String -> [Idris2Def] -> Text
emitOrganIR shimVer modName defs =
    renderOrganIR $ IR.organIR IR.LIdris2 shimVer (T.pack modName) (zipWith defToIR [1 ..] defs)

-- ---------------------------------------------------------------------------
-- Idris 2 case tree parser
-- ---------------------------------------------------------------------------

-- | Parse an Idris 2 case tree expression into OrganIR Expr.
-- Best-effort: falls back to @eApp (eVar "case-tree") [eString text]@.
parseCaseTree :: Text -> IR.Expr
parseCaseTree txt =
    let stripped = T.strip txt
     in case parseExpr (T.unpack stripped) of
            Just (expr, _) -> expr
            Nothing -> caseFallback stripped

-- | Fallback: wrap unparsed text.
caseFallback :: Text -> IR.Expr
caseFallback t = IR.EApp (IR.eVar "case-tree") [IR.eString t]

-- | Try to parse a single expression, returning the expression and remaining input.
parseExpr :: String -> Maybe (IR.Expr, String)
parseExpr s =
    let s' = dropWhile isSpace s
     in case s' of
            -- Lambda: \{arg:N} => body
            ('\\' : rest) ->
                case parseLambda ('\\' : rest) of
                    Just r -> Just r
                    Nothing -> parseSExprOrAtom s'
            -- Parenthesized s-expression: (f a b)
            ('(' : _) -> parseSExprOrAtom s'
            -- case VAR of { ... }
            _ | "case " `isPrefixOf` s' -> parseCase s'
            -- Otherwise: atom (literal, variable, identifier)
            _ -> parseSExprOrAtom s'

-- | Parse lambda chains: \{arg:0} => \{arg:1} => body
parseLambda :: String -> Maybe (IR.Expr, String)
parseLambda s = do
    let s' = dropWhile isSpace s
    -- Expect \{arg:N} =>
    rest0 <- stripPrefix' "\\" s'
    let (param, rest1) = parseVarOrIdent rest0
    rest2 <- stripPrefixWs "=>" rest1
    (body, rest3) <- parseExpr rest2
    let paramName = T.pack param
    pure (IR.ELam [IR.LamParam (IR.Name paramName 0) Nothing] body, rest3)

-- | Parse: case VAR of { ALT ; ALT ; ... }
parseCase :: String -> Maybe (IR.Expr, String)
parseCase s = do
    rest0 <- stripPrefix' "case " s
    let (scrut, rest1) = parseVarOrIdent rest0
    rest2 <- stripPrefixWs "of" rest1
    let rest2' = dropWhile isSpace rest2
    -- Handle both { ... } delimited and indented forms
    case rest2' of
        ('{' : rest3) -> do
            let alts = parseBracedAlts rest3
            let scrutExpr = textToVar (T.pack scrut)
            pure (IR.ECase scrutExpr alts, "")
        _ -> do
            -- Indented alts (no braces) - parse to end
            let alts = parseIndentedAlts rest2'
            let scrutExpr = textToVar (T.pack scrut)
            pure (IR.ECase scrutExpr alts, "")

-- | Parse alts inside { ... }, separated by ;
parseBracedAlts :: String -> [IR.Branch]
parseBracedAlts s =
    let s' = dropWhile isSpace s
     in case s' of
            ('}' : _) -> []
            _ ->
                let (altText, rest) = splitAlt s'
                    b = parseAlt altText
                    rest' = dropWhile isSpace rest
                 in case rest' of
                        (';' : rest'') -> b : parseBracedAlts rest''
                        ('}' : _) -> [b]
                        _ -> [b] -- end of input

-- | Split out one alt from braced form, respecting parens/braces nesting.
splitAlt :: String -> (String, String)
splitAlt = go 0 []
  where
    go :: Int -> String -> String -> (String, String)
    go _ acc [] = (reverse acc, [])
    go 0 acc (';' : rest) = (reverse acc, ';' : rest)
    go 0 acc ('}' : rest) = (reverse acc, '}' : rest)
    go n acc ('(' : rest) = go (n + 1) ('(' : acc) rest
    go n acc (')' : rest) = go (max 0 (n - 1)) (')' : acc) rest
    go n acc ('{' : rest) = go (n + 1) ('{' : acc) rest
    go n acc (c : rest) = go n (c : acc) rest

-- | Parse indented alternatives (no brace delimiters).
parseIndentedAlts :: String -> [IR.Branch]
parseIndentedAlts s =
    let ls = lines s
        altLines = filter (not . null . dropWhile isSpace) ls
     in map (parseAlt . dropWhile isSpace) altLines

-- | Parse a single alternative: pattern => body
parseAlt :: String -> IR.Branch
parseAlt s =
    let s' = dropWhile isSpace s
     in case breakOnArrow s' of
            Just (patStr, bodyStr) ->
                let pat = parsePat (trim' patStr)
                    body = case parseExpr bodyStr of
                        Just (e, _) -> e
                        Nothing -> caseFallback (T.pack bodyStr)
                 in IR.Branch pat body
            Nothing -> IR.Branch IR.PatWild (caseFallback (T.pack s'))

-- | Break on " => " that is not inside parens.
breakOnArrow :: String -> Maybe (String, String)
breakOnArrow = go 0 []
  where
    go :: Int -> String -> String -> Maybe (String, String)
    go _ _ [] = Nothing
    go 0 acc (' ' : '=' : '>' : ' ' : rest) = Just (reverse acc, rest)
    go n acc ('(' : rest) = go (n + 1) ('(' : acc) rest
    go n acc (')' : rest) = go (max 0 (n - 1)) (')' : acc) rest
    go n acc (c : rest) = go n (c : acc) rest

-- | Parse a pattern.
parsePat :: String -> IR.Pat
parsePat "_" = IR.PatWild
parsePat s
    | all isDigit s, not (null s) = IR.PatLit (IR.LitInt (read s))
    | Just s' <- stripPrefix' "-" s, all isDigit s', not (null s') =
        IR.PatLit (IR.LitInt (negate (read s')))
    | Just inner <- stripQuotes s = IR.PatLit (IR.LitString (T.pack inner))
    | '(' : rest <- s =
        -- Constructor with args in parens
        let tokens = tokenizeSExpr rest
         in case tokens of
                (con : args) -> IR.PatCon (IR.localName (T.pack con))
                    (map (\a -> IR.PatBinder (IR.Name (T.pack a) 0) Nothing) args)
                [] -> IR.PatWild
    | otherwise =
        -- Plain constructor or variable
        let (con, rest) = break isSpace s
            args = words rest
         in if null args
                then
                    -- Could be constructor with no args or variable
                    case con of
                        (c : _) | any (== '.') con || isUpper' c ->
                            IR.PatCon (parseQName con) []
                        _ -> IR.PatVar (IR.Name (T.pack con) 0) Nothing
                else IR.PatCon (parseQName con)
                    (map (\a -> IR.PatBinder (IR.Name (T.pack a) 0) Nothing) args)
  where
    isUpper' c = c >= 'A' && c <= 'Z'

-- | Strip surrounding quotes from a string literal.
stripQuotes :: String -> Maybe String
stripQuotes ('"' : rest) = case reverse rest of
    ('"' : inner) -> Just (reverse inner)
    _ -> Nothing
stripQuotes _ = Nothing

-- | Parse an s-expression (f a b) or a single atom.
parseSExprOrAtom :: String -> Maybe (IR.Expr, String)
parseSExprOrAtom s =
    let s' = dropWhile isSpace s
     in case s' of
            ('(' : rest) -> parseSExpr rest
            _ -> parseAtom s'

-- | Parse s-expression contents after opening paren.
parseSExpr :: String -> Maybe (IR.Expr, String)
parseSExpr s =
    let tokens = tokenizeSExpr s
     in case tokens of
            [] -> Nothing
            [single] ->
                let e = textToAtom (T.pack single)
                    rest = dropAfterSExpr s
                 in Just (e, rest)
            (f : args) ->
                let fExpr = textToAtom (T.pack f)
                    argExprs = map (\a -> parseNestedArg a) args
                    rest = dropAfterSExpr s
                 in Just (IR.EApp fExpr argExprs, rest)

-- | Parse a nested argument which could itself be an s-expression.
parseNestedArg :: String -> IR.Expr
parseNestedArg s
    | ('(' : _) <- s = case parseExpr s of
        Just (e, _) -> e
        Nothing -> textToAtom (T.pack s)
    | otherwise = textToAtom (T.pack s)

-- | Tokenize s-expression contents (before closing paren), respecting nesting.
tokenizeSExpr :: String -> [String]
tokenizeSExpr = go 0 [] []
  where
    go :: Int -> String -> [String] -> String -> [String]
    go _ cur acc [] = reverse (finishToken cur acc)
    go 0 cur acc (')' : _) = reverse (finishToken cur acc)
    go n cur acc ('(' : rest) = go (n + 1) ('(' : cur) acc rest
    go n cur acc (')' : rest)
        | n > 0 = go (n - 1) (')' : cur) acc rest
        | otherwise = reverse (finishToken cur acc)
    go 0 cur acc (' ' : rest) = go 0 [] (finishToken cur acc) rest
    go n cur acc (' ' : rest)
        | n > 0 = go n (' ' : cur) acc rest
        | otherwise = go 0 [] (finishToken cur acc) rest
    go n cur acc (c : rest) = go n (c : cur) acc rest
    finishToken [] acc = acc
    finishToken cur acc = reverse cur : acc

-- | Drop input past the closing paren of an s-expression.
dropAfterSExpr :: String -> String
dropAfterSExpr = go (0 :: Int)
  where
    go _ [] = []
    go 0 (')' : rest) = rest
    go n ('(' : rest) = go (n + 1) rest
    go n (')' : rest) = go (n - 1) rest
    go n (_ : rest) = go n rest

-- | Parse a single atom (variable, literal, or identifier).
parseAtom :: String -> Maybe (IR.Expr, String)
parseAtom s =
    let s' = dropWhile isSpace s
     in case s' of
            [] -> Nothing
            ('"' : _) ->
                let (str, rest) = spanString s'
                 in Just (IR.eString (T.pack str), rest)
            _ ->
                let (tok, rest) = spanToken s'
                 in if null tok
                        then Nothing
                        else Just (textToAtom (T.pack tok), rest)

-- | Span a quoted string, handling simple escapes.
spanString :: String -> (String, String)
spanString ('"' : rest) = go [] rest
  where
    go acc [] = (reverse acc, [])
    go acc ('\\' : c : cs) = go (c : '\\' : acc) cs
    go acc ('"' : cs) = (reverse acc, cs)
    go acc (c : cs) = go (c : acc) cs
spanString s = ([], s)

-- | Span a token (non-space, non-paren delimited).
spanToken :: String -> (String, String)
spanToken = span (\c -> not (isSpace c) && c /= ')' && c /= '(' && c /= ';' && c /= '}')

-- | Convert a text token to an atom expression.
textToAtom :: Text -> IR.Expr
textToAtom t
    | Just n <- readInteger t = IR.eInt n
    | t == "True" || t == "true" = IR.eBool True
    | t == "False" || t == "false" = IR.eBool False
    | otherwise = textToVar t

-- | Convert text to a variable expression, handling {arg:N} and qualified names.
textToVar :: Text -> IR.Expr
textToVar t = IR.EVar (IR.Name t 0)

-- | Try to read an integer from text.
readInteger :: Text -> Maybe Integer
readInteger t =
    let s = T.unpack t
     in case s of
            ('-' : rest) | not (null rest), all isDigit rest -> Just (read s)
            _ | not (null s), all isDigit s -> Just (read s)
            _ -> Nothing

-- | Parse a qualified name like "Main.factorial".
parseQName :: String -> IR.QName
parseQName s =
    case breakOnLastDot s of
        Just (modPart, namePart) -> IR.QName (T.pack modPart) (IR.Name (T.pack namePart) 0)
        Nothing -> IR.localName (T.pack s)

-- | Break on the last dot in a string.
breakOnLastDot :: String -> Maybe (String, String)
breakOnLastDot s =
    case break (== '.') (reverse s) of
        (_, []) -> Nothing
        (rname, _ : rmod) -> Just (reverse rmod, reverse rname)

-- | Parse a variable or identifier token (handling {arg:N} syntax).
parseVarOrIdent :: String -> (String, String)
parseVarOrIdent ('{' : rest) =
    let (var, after) = break (== '}') rest
     in case after of
            ('}' : rest') -> ("{" ++ var ++ "}", rest')
            _ -> ("{" ++ var, after)
parseVarOrIdent s = spanToken s

-- | Strip a prefix from a string, or Nothing.
stripPrefix' :: String -> String -> Maybe String
stripPrefix' [] s = Just s
stripPrefix' _ [] = Nothing
stripPrefix' (p : ps) (c : cs)
    | p == c = stripPrefix' ps cs
    | otherwise = Nothing

-- | Strip a prefix after skipping leading whitespace.
stripPrefixWs :: String -> String -> Maybe String
stripPrefixWs prefix s = stripPrefix' prefix (dropWhile isSpace s)

-- | Trim whitespace from both ends.
trim' :: String -> String
trim' = reverse . dropWhile isSpace . reverse . dropWhile isSpace
