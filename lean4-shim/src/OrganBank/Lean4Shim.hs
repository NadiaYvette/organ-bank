{- | Lean 4 LCNF Extraction Shim

Invokes the lean4-organ native binary (built via Lake) to extract
post-erasure LCNF from Lean 4 source files. Also provides a text-based
LCNF expression parser for environments where the native binary emits
textual LCNF rather than fully-structured JSON, or as a fallback.

Lean 4's LCNF (Lambda Compiled Normal Form) text format:
  let x := e; body          -- let binding
  fun x => body             -- lambda
  jp x := e; body           -- join point (treated as let)
  cases x : T { | C a => b; ... }  -- case split
  @C a1 a2                  -- constructor application
  f a1 a2                   -- function application
  #42                       -- numeric literal
  return x                  -- variable return
-}
module OrganBank.Lean4Shim (
    extractOrganIR,
    parseLcnfExpr,
) where

import Control.Exception (SomeException, catch)
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName)
import System.Process (readProcessWithExitCode)

------------------------------------------------------------------------
-- Public entry point
------------------------------------------------------------------------

-- | Detect lean4-organ / lean version.
detectCompilerVersion :: IO Text
detectCompilerVersion = do
    (ec, out, _) <- readProcessWithExitCode "lean" ["--version"] ""
    pure $ case ec of
        ExitSuccess | (l : _) <- lines out -> "lean4-shim-0.1 (" <> T.strip (T.pack l) <> ")"
        _ -> "lean4-shim-0.1"
  `catch` \(_ :: SomeException) -> pure "lean4-shim-0.1"

-- | Extract OrganIR from a Lean 4 source file.
--
-- Strategy: invoke @lean4-organ@ (the native Lean binary that walks the
-- real LCNF AST via the compiler API). If that fails, fall back to
-- running @lean --print-lcnf@ and parsing the text output.
extractOrganIR :: FilePath -> IO (Either Text Text)
extractOrganIR inputPath = do
    shimVer <- detectCompilerVersion
    -- Try native lean4-organ first (structured extraction)
    (ec, stdout_, stderr_) <- readProcessWithExitCode "lean4-organ" [inputPath] ""
    case ec of
        ExitSuccess ->
            -- lean4-organ emits OrganIR JSON directly
            pure $ Right (T.pack stdout_)
        ExitFailure _ -> do
            -- Fallback: parse LCNF text output
            (ec2, stdout2, stderr2) <-
                readProcessWithExitCode "lean" ["--run", inputPath, "--print-lcnf"] ""
            case ec2 of
                ExitSuccess -> do
                    let modName = takeBaseName inputPath
                        defs = parseLcnfDump (T.pack stdout2)
                    pure $ Right $ emitOrganIR shimVer modName defs
                ExitFailure _ ->
                    pure $ Left $ T.pack $
                        "lean4-organ failed: " ++ stderr_
                            ++ "\nlean --print-lcnf failed: " ++ stderr2

------------------------------------------------------------------------
-- LCNF text dump parsing
------------------------------------------------------------------------

-- | A parsed LCNF declaration from textual dump output.
data LcnfDef = LcnfDef
    { lcnfDefName :: Text -- ^ fully qualified name
    , lcnfDefArity :: Int -- ^ number of parameters
    , lcnfDefBodyText :: Text -- ^ raw LCNF body text
    }
    deriving (Show)

-- | Parse textual LCNF dump into definitions.
--
-- Expected format (one decl per block, separated by blank lines):
--
-- @
-- [reducible] def Factorial.factorial._unary (x_1 : @& Nat) : Nat :=
--   let _x.2 := Nat.decEq x_1 0;
--   cases _x.2 : Decidable
--     | Decidable.isTrue h => return 1
--     | Decidable.isFalse h =>
--       let n := Nat.pred x_1;
--       let _x.3 := Factorial.factorial._unary n;
--       let _x.4 := Nat.mul x_1 _x.3;
--       return _x.4
-- @
parseLcnfDump :: Text -> [LcnfDef]
parseLcnfDump raw =
    let blocks = splitBlocks (T.lines raw)
     in concatMap parseBlock blocks

-- | Split lines into blocks separated by empty lines.
splitBlocks :: [Text] -> [[Text]]
splitBlocks [] = []
splitBlocks ls =
    let (block, rest) = span (not . T.null . T.strip) ls
        rest' = dropWhile (T.null . T.strip) rest
     in case block of
            [] -> splitBlocks rest'
            _ -> block : splitBlocks rest'

-- | Parse a single declaration block into a LcnfDef.
parseBlock :: [Text] -> [LcnfDef]
parseBlock [] = []
parseBlock (hdr : bodyLines)
    | Just (defName, arity) <- parseDefHeader hdr =
        let bodyText = T.unlines bodyLines
         in [LcnfDef defName arity bodyText]
    | otherwise = []

-- | Parse a def header line.
--
-- Handles: @[attr] def Name.space (p1 : T1) (p2 : T2) : RetTy :=@
-- Returns: (qualified name, arity)
parseDefHeader :: Text -> Maybe (Text, Int)
parseDefHeader line =
    let stripped = T.strip line
        -- Strip leading attributes like [reducible], [inline], etc.
        noAttrs = stripAttributes stripped
     in case T.stripPrefix "def " (T.strip noAttrs) of
            Just rest ->
                let (nameAndParams, _) = T.breakOn ":=" rest
                    tokens = T.words (T.strip nameAndParams)
                 in case tokens of
                        [] -> Nothing
                        (n : paramTokens) ->
                            let arity = length (filter (\t -> T.isPrefixOf "(" t) paramTokens)
                             in Just (n, arity)
            Nothing -> Nothing

-- | Strip leading @[attr]@ annotations.
stripAttributes :: Text -> Text
stripAttributes t
    | Just rest <- T.stripPrefix "[" t =
        let (_, afterBracket) = T.breakOn "]" rest
         in case T.stripPrefix "]" afterBracket of
                Just r -> stripAttributes (T.strip r)
                Nothing -> t
    | otherwise = t

------------------------------------------------------------------------
-- LCNF expression parser
------------------------------------------------------------------------

-- | Parse a Lean 4 LCNF expression from text.
--
-- Handles the core LCNF constructs:
--
-- * @let x := e; body@ -- let binding
-- * @fun x => body@ -- lambda abstraction
-- * @jp x := e; body@ -- join point (treated as let binding)
-- * @cases x : T { | C args => body; ... }@ -- case expression
-- * @@C a1 a2@ -- constructor application
-- * @f a1 a2@ -- function application
-- * @#lit@ -- numeric literal
-- * @return x@ -- variable reference
--
-- Falls back gracefully to @IR.eApp (IR.eVar "lcnf") [IR.eString rawText]@
-- for forms that cannot be parsed.
parseLcnfExpr :: Text -> IR.Expr
parseLcnfExpr raw
    | T.null stripped = IR.eVar "_"
    -- let x := e; body
    | Just rest <- T.stripPrefix "let " stripped =
        parseLetExpr rest
    -- fun x => body
    | Just rest <- T.stripPrefix "fun " stripped =
        parseFunExpr rest
    -- jp x := e; body  (join point, treated as let)
    | Just rest <- T.stripPrefix "jp " stripped =
        parseJpExpr rest
    -- cases x : T { ... }
    | Just rest <- T.stripPrefix "cases " stripped =
        parseCasesExpr rest
    -- @Ctor args  (constructor application)
    | Just rest <- T.stripPrefix "@" stripped
    , not (T.null rest)
    , Char.isUpper (T.head rest) =
        parseConApp rest
    -- #literal
    | Just rest <- T.stripPrefix "#" stripped =
        parseLitExpr rest
    -- return x
    | Just rest <- T.stripPrefix "return " stripped =
        IR.eVar (T.strip rest)
    -- String literal
    | "\"" `T.isPrefixOf` stripped =
        case T.stripPrefix "\"" stripped of
            Just afterQuote ->
                let (content, _) = T.breakOn "\"" afterQuote
                 in IR.ELit (IR.LitString content)
            Nothing -> fallbackExpr stripped
    -- Numeric literal (bare, without #)
    | isNumericStart stripped =
        parseNumericLit stripped
    -- Function application: f a1 a2
    -- or bare identifier
    | isIdentStart stripped =
        parseAppOrVar stripped
    -- Fallback
    | otherwise = fallbackExpr stripped
  where
    stripped = T.strip raw

-- | Fallback: wrap unparsed LCNF text.
fallbackExpr :: Text -> IR.Expr
fallbackExpr t = IR.eApp (IR.eVar "lcnf") [IR.eString t]

------------------------------------------------------------------------
-- let x := e; body
------------------------------------------------------------------------

-- | Parse @let x := e; body@ or multi-line let.
parseLetExpr :: Text -> IR.Expr
parseLetExpr rest =
    case T.breakOn ":=" rest of
        (lhs, rhs)
            | not (T.null rhs) ->
                let bindName = T.strip lhs
                    afterAssign = T.drop 2 rhs -- drop ":="
                    (valText, bodyText) = splitLetBody afterAssign
                 in IR.ELet
                        [IR.LetBind (IR.Name bindName 0) Nothing (parseLcnfExpr valText)]
                        (parseLcnfExpr bodyText)
            | otherwise -> fallbackExpr ("let " <> rest)

-- | Split a let binding value from its continuation body.
--
-- The value ends at the first top-level @;@ (same line) or at the next
-- line that starts with @let@, @jp@, @cases@, @return@, @fun@ (multi-line
-- LCNF format).
splitLetBody :: Text -> (Text, Text)
splitLetBody t =
    let stripped = T.strip t
     in case splitAtTopLevelSemicolon stripped of
            Just (val, body) -> (T.strip val, T.strip body)
            Nothing ->
                -- Multi-line: look for continuation keywords
                splitAtContinuation stripped

-- | Split at the first top-level semicolon (not inside braces/parens).
splitAtTopLevelSemicolon :: Text -> Maybe (Text, Text)
splitAtTopLevelSemicolon = go 0 T.empty
  where
    go :: Int -> Text -> Text -> Maybe (Text, Text)
    go !_depth _acc rest | T.null rest = Nothing
    go !depth acc rest =
        let c = T.head rest
            rest' = T.tail rest
         in case c of
                ';' | depth == 0 -> Just (acc, rest')
                '(' -> go (depth + 1) (T.snoc acc c) rest'
                ')' -> go (max 0 (depth - 1)) (T.snoc acc c) rest'
                '{' -> go (depth + 1) (T.snoc acc c) rest'
                '}' -> go (max 0 (depth - 1)) (T.snoc acc c) rest'
                _ -> go depth (T.snoc acc c) rest'

-- | Split at the next LCNF continuation keyword on a new line.
--
-- In multi-line format, each let value is on the same line, and the
-- continuation starts on the next line with a keyword.
splitAtContinuation :: Text -> (Text, Text)
splitAtContinuation t =
    let ls = T.lines t
     in case ls of
            [] -> (t, "")
            [single] -> (single, "")
            (first : rest) ->
                -- The first line is the value; find where continuation starts
                let (valLines, bodyLines) = spanValueLines rest
                 in ( T.unlines (first : valLines)
                    , T.unlines bodyLines
                    )

-- | Span lines that are part of the current value (indented, not a keyword).
spanValueLines :: [Text] -> ([Text], [Text])
spanValueLines [] = ([], [])
spanValueLines (l : ls)
    | isContinuationKeyword (T.strip l) = ([], l : ls)
    | otherwise =
        let (more, rest) = spanValueLines ls
         in (l : more, rest)

-- | Does this line start with an LCNF continuation keyword?
isContinuationKeyword :: Text -> Bool
isContinuationKeyword t =
    any (\kw -> kw `T.isPrefixOf` t)
        ["let ", "jp ", "cases ", "return ", "fun "]

------------------------------------------------------------------------
-- fun x => body
------------------------------------------------------------------------

-- | Parse @fun x => body@.
parseFunExpr :: Text -> IR.Expr
parseFunExpr rest =
    case breakAtTopLevelArrow rest of
        Just (paramText, bodyText) ->
            let params = parseLamParams paramText
             in IR.ELam params (parseLcnfExpr bodyText)
        Nothing -> fallbackExpr ("fun " <> rest)

-- | Break at top-level @=>@ (not inside braces/parens).
breakAtTopLevelArrow :: Text -> Maybe (Text, Text)
breakAtTopLevelArrow = go 0 T.empty
  where
    go :: Int -> Text -> Text -> Maybe (Text, Text)
    go !_depth _acc rest | T.null rest = Nothing
    go !depth acc rest
        | "=>" `T.isPrefixOf` rest && depth == 0 =
            Just (acc, T.drop 2 rest)
        | otherwise =
            let c = T.head rest
                rest' = T.tail rest
                depth' = adjustDepth c depth
             in go depth' (T.snoc acc c) rest'

-- | Parse lambda parameters from text like @x@ or @(x : T) (y : U)@.
parseLamParams :: Text -> [IR.LamParam]
parseLamParams t =
    let stripped = T.strip t
     in if T.null stripped
            then []
            else map parseOneLamParam (splitParamGroups stripped)

-- | Split parameter groups: @(x : T) (y : U)@ or bare @x y@.
splitParamGroups :: Text -> [Text]
splitParamGroups t
    | T.null (T.strip t) = []
    | "(" `T.isPrefixOf` T.strip t =
        let (inside, rest) = matchParens (T.strip t)
         in inside : splitParamGroups rest
    | otherwise =
        let (word, rest) = T.break Char.isSpace (T.strip t)
         in if T.null word
                then []
                else word : splitParamGroups rest

-- | Parse a single parameter: @x : T@ or bare @x@.
parseOneLamParam :: Text -> IR.LamParam
parseOneLamParam t =
    let stripped = T.strip t
     in case T.breakOn ":" stripped of
            (n, rest)
                | not (T.null rest) && not (":" `T.isPrefixOf` T.strip n) ->
                    -- Strip the leading ":" and optional "@& " (borrow annotation)
                    let tyText = T.strip (T.drop 1 rest)
                        tyClean = case T.stripPrefix "@& " tyText of
                            Just r -> r
                            Nothing -> tyText
                     in IR.LamParam (IR.Name (T.strip n) 0) (Just (IR.tCon tyClean))
                | otherwise ->
                    IR.LamParam (IR.Name stripped 0) Nothing

------------------------------------------------------------------------
-- jp x := e; body  (join point)
------------------------------------------------------------------------

-- | Parse @jp x := e; body@ (join point, treated as a let binding).
parseJpExpr :: Text -> IR.Expr
parseJpExpr rest =
    -- Join points have the same syntax as let, possibly with parameters:
    --   jp _jp.1 (x : T) := body; continuation
    -- We treat them as let-bound lambdas.
    case T.breakOn ":=" rest of
        (lhs, rhs)
            | not (T.null rhs) ->
                let (jpName, paramText) = splitJpNameParams (T.strip lhs)
                    afterAssign = T.drop 2 rhs
                    (valText, bodyText) = splitLetBody afterAssign
                    jpBody = parseLcnfExpr valText
                    jpExpr = case parseLamParams paramText of
                        [] -> jpBody
                        params -> IR.ELam params jpBody
                 in IR.ELet
                        [IR.LetBind (IR.Name jpName 0) Nothing jpExpr]
                        (parseLcnfExpr bodyText)
            | otherwise -> fallbackExpr ("jp " <> rest)

-- | Split @_jp.1 (x : T) (y : U)@ into @("_jp.1", "(x : T) (y : U)")@.
splitJpNameParams :: Text -> (Text, Text)
splitJpNameParams t =
    let (n, rest) = T.break (\c -> c == '(' || Char.isSpace c) t
     in (T.strip n, T.strip rest)

------------------------------------------------------------------------
-- cases x : T { | C args => body; ... }
------------------------------------------------------------------------

-- | Parse @cases x : T { | C args => body; ... }@ or the indented form.
parseCasesExpr :: Text -> IR.Expr
parseCasesExpr rest =
    let (scrutAndType, branchText) = splitCasesParts rest
        scrutName = parseScrutinee scrutAndType
        scrut = IR.eVar scrutName
        branches = parseCaseBranches branchText
     in IR.ECase scrut branches

-- | Split @x : T { ... }@ or multi-line cases into (scrutinee+type, branches).
splitCasesParts :: Text -> (Text, Text)
splitCasesParts t =
    -- Try brace-delimited form: cases x : T { ... }
    case T.breakOn "{" t of
        (before, after)
            | not (T.null after) ->
                let (inside, _) = matchBraces (T.drop 1 after)
                 in (T.strip before, T.strip inside)
            | otherwise ->
                -- Indented form: branches on following lines starting with |
                let ls = T.lines t
                 in case ls of
                        [] -> (t, "")
                        (hdr : brs) -> (T.strip hdr, T.unlines brs)

-- | Extract scrutinee name from @x : T@ or @x@.
parseScrutinee :: Text -> Text
parseScrutinee t =
    let stripped = T.strip t
     in case T.breakOn ":" stripped of
            (n, rest)
                | not (T.null rest) -> T.strip n
                | otherwise -> stripped

-- | Parse case branches from text inside @{ ... }@ or indented lines.
--
-- Each branch: @| Ctor args => body@ or @| _ => body@
parseCaseBranches :: Text -> [IR.Branch]
parseCaseBranches t =
    let rawBranches = splitOnPipe t
     in concatMap parseOneCaseBranch rawBranches

-- | Split on top-level @|@ delimiters, respecting nesting.
splitOnPipe :: Text -> [Text]
splitOnPipe t =
    let -- Split on lines starting with |, or on top-level | in single-line form
        ls = T.lines t
     in if any (\l -> "|" `T.isPrefixOf` T.strip l) ls
            then -- Multi-line form with | at line starts
                groupPipeBranches ls
            else -- Single-line: split at top-level |
                splitTopLevelPipe t

-- | Group lines into branches starting at @|@.
groupPipeBranches :: [Text] -> [Text]
groupPipeBranches [] = []
groupPipeBranches (l : ls)
    | "|" `T.isPrefixOf` T.strip l =
        let (contLines, rest) = span (\l' -> not ("|" `T.isPrefixOf` T.strip l')) ls
         in T.unlines (l : contLines) : groupPipeBranches rest
    | otherwise = groupPipeBranches ls

-- | Split at top-level @|@ characters (not inside braces/parens).
splitTopLevelPipe :: Text -> [Text]
splitTopLevelPipe = go 0 T.empty
  where
    go :: Int -> Text -> Text -> [Text]
    go !_depth acc rest | T.null rest =
        [acc | not (T.null (T.strip acc))]
    go !depth acc rest =
        let c = T.head rest
            rest' = T.tail rest
         in case c of
                '|' | depth == 0 ->
                    (if T.null (T.strip acc) then id else (acc :))
                        (go 0 T.empty rest')
                '(' -> go (depth + 1) (T.snoc acc c) rest'
                ')' -> go (max 0 (depth - 1)) (T.snoc acc c) rest'
                '{' -> go (depth + 1) (T.snoc acc c) rest'
                '}' -> go (max 0 (depth - 1)) (T.snoc acc c) rest'
                _ -> go depth (T.snoc acc c) rest'

-- | Parse a single case branch: @| Ctor args => body@ or @| _ => body@.
parseOneCaseBranch :: Text -> [IR.Branch]
parseOneCaseBranch t =
    let stripped = T.strip t
        -- Strip leading |
        noPipe = case T.stripPrefix "|" stripped of
            Just r -> T.strip r
            Nothing -> stripped
     in case breakAtTopLevelArrow noPipe of
            Just (patText, bodyText) ->
                [IR.Branch
                    { IR.brPattern = parseLcnfPattern (T.strip patText)
                    , IR.brBody = parseLcnfExpr (T.strip bodyText)
                    }]
            Nothing -> []

-- | Parse an LCNF pattern: @Ctor a b@, @_@, or a literal.
parseLcnfPattern :: Text -> IR.Pat
parseLcnfPattern t
    | T.null t = IR.PatWild
    | t == "_" = IR.PatWild
    -- Constructor pattern: starts with uppercase
    | Char.isUpper (T.head t) =
        let ws = T.words t
         in case ws of
                [] -> IR.PatWild
                (ctor : args) ->
                    IR.PatCon
                        (parseQualName ctor)
                        (map (\a -> IR.PatBinder (IR.Name a 0) Nothing) args)
    -- Numeric literal pattern
    | isNumericStart t =
        case parseNumericLit t of
            IR.ELit lit -> IR.PatLit lit
            _ -> IR.PatVar (IR.Name t 0) Nothing
    -- Variable pattern
    | otherwise = IR.PatVar (IR.Name t 0) Nothing

------------------------------------------------------------------------
-- @Ctor a1 a2  (constructor application)
------------------------------------------------------------------------

-- | Parse @@Ctor a1 a2@ as a constructor application.
parseConApp :: Text -> IR.Expr
parseConApp rest =
    let ws = T.words rest
     in case ws of
            [] -> fallbackExpr ("@" <> rest)
            (ctor : args) ->
                IR.ECon (parseQualName ctor) (map parseLcnfExpr args)

------------------------------------------------------------------------
-- #literal
------------------------------------------------------------------------

-- | Parse @#42@, @#3.14@, @#\"str\"@.
parseLitExpr :: Text -> IR.Expr
parseLitExpr rest
    | T.null rest = fallbackExpr "#"
    -- String literal: #"..."
    | "\"" `T.isPrefixOf` rest =
        case T.stripPrefix "\"" rest of
            Just afterQuote ->
                let (content, _) = T.breakOn "\"" afterQuote
                 in IR.ELit (IR.LitString content)
            Nothing -> fallbackExpr ("#" <> rest)
    -- Numeric
    | otherwise = parseNumericLit rest

------------------------------------------------------------------------
-- Function application / variable
------------------------------------------------------------------------

-- | Parse @f a1 a2@ (application) or bare @x@ (variable).
parseAppOrVar :: Text -> IR.Expr
parseAppOrVar t =
    let ws = splitTopLevelWords t
     in case ws of
            [] -> fallbackExpr t
            [single] -> IR.eVar single
            (fn : args) -> IR.EApp (IR.eVar fn) (map parseLcnfExpr args)

-- | Split at top-level spaces, respecting parenthesized groups.
splitTopLevelWords :: Text -> [Text]
splitTopLevelWords = go 0 T.empty
  where
    go :: Int -> Text -> Text -> [Text]
    go !_depth acc rest | T.null rest =
        [acc | not (T.null (T.strip acc))]
    go !depth acc rest =
        let c = T.head rest
            rest' = T.tail rest
         in case c of
                ' ' | depth == 0 && not (T.null (T.strip acc)) ->
                    T.strip acc : go 0 T.empty rest'
                '(' -> go (depth + 1) (T.snoc acc c) rest'
                ')' -> go (max 0 (depth - 1)) (T.snoc acc c) rest'
                '{' -> go (depth + 1) (T.snoc acc c) rest'
                '}' -> go (max 0 (depth - 1)) (T.snoc acc c) rest'
                _ -> go depth (T.snoc acc c) rest'

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Parse a potentially qualified name: @Foo.Bar.baz@ -> QName.
parseQualName :: Text -> IR.QName
parseQualName t =
    case T.breakOnEnd "." t of
        (modPart, localPart)
            | not (T.null modPart) && not (T.null localPart) ->
                IR.QName (T.dropEnd 1 modPart) (IR.Name localPart 0)
            | otherwise ->
                IR.localName t

-- | Match balanced parentheses: @(...)rest@ -> @(inside, rest)@.
matchParens :: Text -> (Text, Text)
matchParens t
    | Just inner <- T.stripPrefix "(" t = go 1 T.empty inner
    | otherwise = (t, T.empty)
  where
    go :: Int -> Text -> Text -> (Text, Text)
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

-- | Match balanced braces: @{...}rest@ -> @(inside, rest)@.
matchBraces :: Text -> (Text, Text)
matchBraces = go 1 T.empty
  where
    go :: Int -> Text -> Text -> (Text, Text)
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

-- | Adjust nesting depth for a character.
adjustDepth :: Char -> Int -> Int
adjustDepth '(' d = d + 1
adjustDepth ')' d = max 0 (d - 1)
adjustDepth '{' d = d + 1
adjustDepth '}' d = max 0 (d - 1)
adjustDepth _ d = d

-- | Check if text starts with a digit or negative sign + digit.
isNumericStart :: Text -> Bool
isNumericStart t = case T.uncons t of
    Just (c, _) | Char.isDigit c -> True
    Just ('-', rest) -> case T.uncons rest of
        Just (c, _) | Char.isDigit c -> True
        _ -> False
    _ -> False

-- | Check if text starts with an identifier character.
isIdentStart :: Text -> Bool
isIdentStart t = case T.uncons t of
    Just (c, _) -> Char.isAlpha c || c == '_'
    Nothing -> False

-- | Parse a numeric literal (integer or float).
parseNumericLit :: Text -> IR.Expr
parseNumericLit t =
    let numText = T.takeWhile (\c -> Char.isDigit c || c == '.' || c == '-' || c == 'e' || c == 'E') t
     in if "." `T.isInfixOf` numText || "e" `T.isInfixOf` numText || "E" `T.isInfixOf` numText
            then case reads (T.unpack numText) :: [(Double, String)] of
                [(d, "")] -> IR.ELit (IR.LitFloat d)
                _ -> fallbackExpr t
            else case reads (T.unpack numText) :: [(Integer, String)] of
                [(n, "")] -> IR.ELit (IR.LitInt n)
                _ -> fallbackExpr t

------------------------------------------------------------------------
-- LCNF dump -> OrganIR conversion
------------------------------------------------------------------------

-- | Convert parsed LCNF definitions to OrganIR JSON text.
emitOrganIR :: Text -> String -> [LcnfDef] -> Text
emitOrganIR shimVer modName defs =
    renderOrganIR $
        IR.simpleOrganIR IR.LLean4 shimVer (T.pack modName) (modName ++ ".lean") $
            zipWith defToIR [1 ..] defs

-- | Convert a single LcnfDef to an OrganIR Definition.
defToIR :: Int -> LcnfDef -> IR.Definition
defToIR uid def' =
    let qname = IR.QName "" (IR.Name (lcnfDefName def') (fromIntegral uid))
        arity = lcnfDefArity def'
        ty
            | arity == 0 = IR.TAny
            | otherwise =
                IR.TFn
                    (replicate arity (IR.FnArg (Just IR.Many) IR.TAny))
                    IR.pureEffect
                    IR.TAny
        bodyExpr
            | T.null (T.strip (lcnfDefBodyText def')) =
                IR.eApp (IR.eVar "lcnf") [IR.eString "<empty>"]
            | otherwise = parseLcnfExpr (lcnfDefBodyText def')
     in IR.Definition
            { IR.defName = qname
            , IR.defType = ty
            , IR.defExpr = bodyExpr
            , IR.defSort = IR.SFun
            , IR.defVisibility = IR.Public
            , IR.defArity = arity
            }
