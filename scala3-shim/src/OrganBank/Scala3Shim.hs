{- | Scala 3 Post-Erasure Tree Extraction Shim

Runs @scala compile -Vprint:erasure@ on a Scala source file, parses the
post-erasure typed tree text output, and emits OrganIR JSON on stdout.

After erasure, Scala 3's trees show:
  - Monomorphized types (no generics)
  - Dot-notation method calls: @n.*(m)@, @n.-(1)@, @n.<=(1)@
  - Explicit boxing: @Int.box(...)@
  - Module-qualified calls: @ModuleName.method(args)@
-}
module OrganBank.Scala3Shim (
    extractOrganIR,
) where

import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName)
import System.Process (readProcessWithExitCode)

-- ---------------------------------------------------------------------------
-- Types for parsed Scala erasure trees
-- ---------------------------------------------------------------------------

data ScalaDef = ScalaDef
    { sdName :: !Text
    -- ^ Method name
    , sdParams :: ![(Text, Text)]
    -- ^ (paramName, paramType) pairs
    , sdReturnType :: !Text
    -- ^ Return type
    , sdBody :: !ScalaExpr
    -- ^ Parsed body expression
    , sdVisibility :: !Text
    -- ^ "public" or "private"
    }

data ScalaExpr
    = SELitInt !Int
    | SELitStr !Text
    | SEVar !Text
    | -- | Function application
      SEApp !ScalaExpr ![ScalaExpr]
    | -- | Binary primitive: op, lhs, rhs
      SEPrim !Text !ScalaExpr !ScalaExpr
    | -- | if/then/else
      SEIf !ScalaExpr !ScalaExpr !ScalaExpr
    | -- | val binding
      SELet !Text !ScalaExpr !ScalaExpr
    | -- | new Constructor(args)
      SENew !Text ![ScalaExpr]
    | -- | { stmts; expr }
      SEBlock ![ScalaExpr]
    | -- | Fallback for unparsed fragments
      SEUnparsed !Text

data ScalaClass = ScalaClass
    { _scName :: !Text
    , scDefs :: ![ScalaDef]
    }

-- ---------------------------------------------------------------------------
-- Top-level entry point
-- ---------------------------------------------------------------------------

scalaBin :: FilePath
scalaBin = "/home/nyc/.local/share/coursier/bin/scala"

{- | Extract OrganIR JSON from a Scala source file.

Note: @scala compile -Vprint:erasure@ emits the tree dump on stderr,
not stdout. We read stderr for the tree output.
-}
extractOrganIR :: FilePath -> IO (Either Text Text)
extractOrganIR inputPath = do
    (exitCode, _stdoutText, stderrText) <-
        readProcessWithExitCode
            scalaBin
            ["compile", "-Vprint:erasure", inputPath]
            ""
    let cleaned = stripAnsi (T.pack stderrText)
        hasTree = T.isInfixOf "[[syntax trees" cleaned
    case exitCode of
        ExitSuccess
            | hasTree -> do
                let modName = takeBaseName inputPath
                    classes = parseErasureOutput cleaned
                pure $ Right $ emitOrganIR modName inputPath classes
            | otherwise ->
                pure $ Left "scala compile succeeded but no erasure tree in output"
        ExitFailure code ->
            -- Scala may exit non-zero but still dump the tree on stderr.
            if hasTree
                then do
                    let modName = takeBaseName inputPath
                        classes = parseErasureOutput cleaned
                    pure $ Right $ emitOrganIR modName inputPath classes
                else
                    pure $
                        Left $
                            T.unlines
                                [ "scala compile -Vprint:erasure failed (exit " <> T.pack (show code) <> ")"
                                , T.pack stderrText
                                ]

-- ---------------------------------------------------------------------------
-- ANSI escape stripping
-- ---------------------------------------------------------------------------

-- | Strip ANSI escape sequences (color codes) from compiler output.
stripAnsi :: Text -> Text
stripAnsi t
    | T.null t = T.empty
    | otherwise =
        case T.uncons t of
            Nothing -> T.empty
            Just ('\x1b', rest) ->
                -- Skip ESC [ ... <letter>
                case T.uncons rest of
                    Just ('[', rest2) -> stripAnsi (T.dropWhile (\c -> c /= 'm' && c /= 'K' && c /= 'J' && c /= 'H' && c /= 'A' && c /= 'B' && c /= 'C' && c /= 'D') rest2 & T.drop 1)
                    _ -> stripAnsi rest
            Just (c, rest) -> T.cons c (stripAnsi rest)
  where
    (&) = flip ($)

-- ---------------------------------------------------------------------------
-- Erasure Tree Parser
-- ---------------------------------------------------------------------------

-- | Parse the full erasure output into class definitions.
parseErasureOutput :: Text -> [ScalaClass]
parseErasureOutput raw =
    let ls = T.lines raw
        -- Skip header/footer/message lines
        bodyLines = filter (not . isHeaderOrMessage) ls
        -- Join continuation lines: if a line doesn't end with { or } and
        -- the next line is more indented, merge them.
        joined = joinContinuations bodyLines
     in parseClasses (T.unlines joined)

-- | Check if a line is a header/message we should skip.
isHeaderOrMessage :: Text -> Bool
isHeaderOrMessage line =
    let s = T.strip line
     in T.isPrefixOf "[[syntax trees" s
            || T.isPrefixOf "Compiling " s
            || T.isPrefixOf "Compiled " s
            || T.null s

{- | Join continuation lines where a class/def header spans multiple lines.
The Scala compiler wraps long lines, e.g.:
  @SourceFile("...") final module class ScalaTest() extends
      Object() {
We join lines where the current line does not contain '{' and the next
line is a continuation (starts with more indentation and contains '{').
-}
joinContinuations :: [Text] -> [Text]
joinContinuations [] = []
joinContinuations [l] = [l]
joinContinuations (l1 : l2 : rest)
    | isContinuation l1 l2 =
        joinContinuations ((l1 <> " " <> T.strip l2) : rest)
    | otherwise = l1 : joinContinuations (l2 : rest)
  where
    isContinuation a b =
        let sa = T.strip a
            sb = T.strip b
         in not (T.null sa)
                && not (T.null sb)
                -- Current line has "extends" or "class" but no opening brace
                && (T.isInfixOf "extends" sa || T.isInfixOf "class " sa)
                && not (T.isInfixOf "{" sa)
                -- Next line has the opening brace
                && T.isInfixOf "{" sb

-- | Parse class/object definitions from the erasure output.
parseClasses :: Text -> [ScalaClass]
parseClasses body =
    let ls = T.lines body
     in go ls
  where
    go [] = []
    go (l : rest)
        | isClassHeader l =
            let className = extractClassName l
                (bodyLines, remaining) = collectBraceBlock rest
                defs = parseDefs bodyLines
             in ScalaClass className defs : go remaining
        | isModuleVal l = go rest -- skip "lazy module val ..." lines
        | isPackageLine l =
            -- Enter the package block, parse classes inside
            let (bodyLines, remaining) = collectBraceBlock rest
             in parseClasses (T.unlines bodyLines) ++ go remaining
        | otherwise = go rest

-- | Check if a line starts a class/object definition.
isClassHeader :: Text -> Bool
isClassHeader line =
    let s = T.strip line
     in (T.isInfixOf "class " s || T.isInfixOf "object " s)
            && T.isInfixOf "{" s
            && not (T.isPrefixOf "classOf" s)

-- | Check if a line is a package declaration with a brace.
isPackageLine :: Text -> Bool
isPackageLine line =
    let s = T.strip line
     in T.isPrefixOf "package " s && T.isInfixOf "{" s

-- | Check if a line is a module val declaration.
isModuleVal :: Text -> Bool
isModuleVal line = T.isInfixOf "module val " (T.strip line)

-- | Extract the class/object name from a header line.
extractClassName :: Text -> Text
extractClassName line =
    let s = T.strip line
        -- Find "class Name" or "object Name"
        tokens = T.words s
     in findNameAfter ["class", "object"] tokens
  where
    findNameAfter _ [] = "<unknown>"
    findNameAfter keywords (w : ws)
        | w `elem` keywords = case ws of
            (name : _) -> T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '$') name
            _ -> "<unknown>"
        | otherwise = findNameAfter keywords ws

{- | Collect lines inside a brace-delimited block.
Assumes the opening { was on the previous line.
-}
collectBraceBlock :: [Text] -> ([Text], [Text])
collectBraceBlock = go 1 []
  where
    go :: Int -> [Text] -> [Text] -> ([Text], [Text])
    go _ acc [] = (reverse acc, [])
    go depth acc (l : ls)
        | depth <= 0 = (reverse acc, l : ls)
        | otherwise =
            let opens = T.count "{" l
                closes = T.count "}" l
                depth' = depth + opens - closes
             in if depth' <= 0
                    then (reverse acc, ls)
                    else go depth' (l : acc) ls

-- | Parse def definitions from class body lines.
parseDefs :: [Text] -> [ScalaDef]
parseDefs = go
  where
    go [] = []
    go (l : rest)
        | isDefLine l =
            let (vis, name, params, retTy, bodyStart) = parseDefHeader l
                -- Body might be on the same line or span multiple lines
                (bodyText, remaining) = collectDefBody bodyStart rest
                expr = parseExpr (T.strip bodyText)
             in ScalaDef
                    { sdName = name
                    , sdParams = params
                    , sdReturnType = retTy
                    , sdBody = expr
                    , sdVisibility = vis
                    }
                    : go remaining
        | otherwise = go rest

-- | Check if a line contains a def definition.
isDefLine :: Text -> Bool
isDefLine line =
    let s = T.strip line
     in T.isInfixOf "def " s
            && T.isInfixOf ": " s
            && not (T.isPrefixOf "//" s)

{- | Parse a def header line:
  def factorial(n: Int): Int =
  private def writeReplace(): Object =
-}
parseDefHeader :: Text -> (Text, Text, [(Text, Text)], Text, Text)
parseDefHeader line =
    let s = T.strip line
        -- Extract visibility
        (vis, afterVis)
            | T.isPrefixOf "private " s = ("private", T.strip $ T.drop 8 s)
            | T.isPrefixOf "protected " s = ("private", T.strip $ T.drop 10 s)
            | otherwise = ("public", s)
        -- Skip to "def"
        afterDef = case T.breakOn "def " afterVis of
            (_, rest) -> T.drop 4 rest
        -- Extract name (up to '(')
        name = T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '$') afterDef
        afterName = T.drop (T.length name) afterDef
        -- Extract parameter list
        (params, afterParams) = parseParamList afterName
        -- Extract return type: ": RetType ="
        (retTy, bodyStart) = parseReturnAndBody afterParams
     in (vis, name, params, retTy, bodyStart)

{- | Parse a parenthesized parameter list.
Input: "(n: Int, m: String): ..."
Returns: ([(name, type)], remaining text after ")")
-}
parseParamList :: Text -> ([(Text, Text)], Text)
parseParamList t
    | Just rest <- T.stripPrefix "(" (T.strip t) =
        let (inner, afterParen) = findCloseParen rest 1
            params = parseParams inner
         in (params, afterParen)
    | otherwise = ([], t)

-- | Find matching close paren and return (inside, after-paren).
findCloseParen :: Text -> Int -> (Text, Text)
findCloseParen t depth
    | T.null t = ("", "")
    | depth == 0 = ("", t)
    | otherwise =
        case T.uncons t of
            Nothing -> ("", "")
            Just ('(', rest) ->
                let (inner, remaining) = findCloseParen rest (depth + 1)
                 in (T.cons '(' inner, remaining)
            Just (')', rest)
                | depth == 1 -> ("", rest)
                | otherwise ->
                    let (inner, remaining) = findCloseParen rest (depth - 1)
                     in (T.cons ')' inner, remaining)
            Just (c, rest) ->
                let (inner, remaining) = findCloseParen rest depth
                 in (T.cons c inner, remaining)

-- | Parse "name: Type, name2: Type2" into [(name, type)] pairs.
parseParams :: Text -> [(Text, Text)]
parseParams t
    | T.null (T.strip t) = []
    | otherwise = map parseOneParam (splitOnComma (T.strip t))

-- | Split on commas, respecting nesting.
splitOnComma :: Text -> [Text]
splitOnComma = go 0 T.empty []
  where
    go :: Int -> Text -> [Text] -> Text -> [Text]
    go _ acc result t
        | T.null t = reverse (acc : result)
    go depth acc result t =
        case T.uncons t of
            Nothing -> reverse (acc : result)
            Just ('(', rest) -> go (depth + 1) (T.snoc acc '(') result rest
            Just (')', rest) -> go (depth - 1) (T.snoc acc ')') result rest
            Just ('[', rest) -> go (depth + 1) (T.snoc acc '[') result rest
            Just (']', rest) -> go (depth - 1) (T.snoc acc ']') result rest
            Just (',', rest) | depth == 0 -> go 0 T.empty (acc : result) rest
            Just (c, rest) -> go depth (T.snoc acc c) result rest

-- | Parse "name: Type" into (name, type).
parseOneParam :: Text -> (Text, Text)
parseOneParam t =
    let s = T.strip t
     in case T.breakOn ": " s of
            (name, rest)
                | T.null rest -> (s, "Any")
                | otherwise -> (T.strip name, T.strip (T.drop 2 rest))

-- | Parse ": RetType = body" into (retType, bodyStart).
parseReturnAndBody :: Text -> (Text, Text)
parseReturnAndBody t =
    let s = T.strip t
     in case T.stripPrefix ":" s of
            Just afterColon ->
                let ac = T.strip afterColon
                 in case T.breakOn " =" ac of
                        (retTy, eqRest)
                            | T.null eqRest -> (T.strip ac, "")
                            | otherwise -> (T.strip retTy, T.strip (T.drop 2 eqRest))
            Nothing ->
                -- Maybe just "= body" with no explicit return type
                case T.breakOn "= " s of
                    (_, rest)
                        | T.null rest -> ("Unit", "")
                        | otherwise -> ("Unit", T.drop 2 rest)

{- | Collect the body of a def. The body might start on the same line as
the header, or continue on subsequent indented lines.
-}
collectDefBody :: Text -> [Text] -> (Text, [Text])
collectDefBody bodyStart rest
    | T.null (T.strip bodyStart) =
        -- Body is on the next lines — collect indented continuation
        let (bodyLines, remaining) = span isIndentedOrBlock rest
         in (T.unlines bodyLines, remaining)
    | T.isInfixOf "{" bodyStart =
        -- Body starts a block — collect matching braces
        let (blockLines, remaining) = collectBraceBlock rest
         in (bodyStart <> "\n" <> T.unlines blockLines, remaining)
    | otherwise = (bodyStart, rest)

-- | Check if a line is an indented continuation (not a new def/class).
isIndentedOrBlock :: Text -> Bool
isIndentedOrBlock line
    | T.null line = False
    | otherwise =
        let s = T.strip line
         in (T.head line == ' ' && countLeadingSpaces line > 4)
                && not (isDefLine line && not (T.isPrefixOf "if " s))
                && not (isClassHeader line)

countLeadingSpaces :: Text -> Int
countLeadingSpaces = T.length . T.takeWhile (== ' ')

-- ---------------------------------------------------------------------------
-- Expression Parser
-- ---------------------------------------------------------------------------

{- | Parse an expression from the erasure tree text.
The post-erasure output uses infix operators: @n <= 1@, @n * f(x)@
so we must handle precedence. We use a simple two-level approach:
comparison ops bind loosest, then arithmetic.
-}
parseExpr :: Text -> ScalaExpr
parseExpr t
    | T.null s = SEUnparsed ""
    | otherwise = parseInfix s
  where
    s = T.strip t

{- | Parse infix expressions with precedence.
But first check for compound expressions that should not be split by infix.
-}
parseInfix :: Text -> ScalaExpr
parseInfix t
    | T.isPrefixOf "if " s = parseIfExpr s
    | T.isPrefixOf "val " s = parseValExpr s
    | T.isPrefixOf "{" s = parseBlockExpr s
    | otherwise = parseCmpLevel s
  where
    s = T.strip t

-- | Parse comparison level: split on <= >= < > == !=
parseCmpLevel :: Text -> ScalaExpr
parseCmpLevel t =
    case findInfixOp cmpOps t of
        Just (lhs, op, rhs) -> SEPrim (normalizeOp op) (parseAddLevel lhs) (parseAddLevel rhs)
        Nothing -> parseAddLevel t
  where
    cmpOps = ["<=", ">=", "!=", "==", "<", ">"]

-- | Parse additive level: split on + -
parseAddLevel :: Text -> ScalaExpr
parseAddLevel t =
    case findInfixOp addOps t of
        Just (lhs, op, rhs) -> SEPrim (normalizeOp op) (parseMulLevel lhs) (parseMulLevel rhs)
        Nothing -> parseMulLevel t
  where
    addOps = ["+", "-"]

-- | Parse multiplicative level: split on * / %
parseMulLevel :: Text -> ScalaExpr
parseMulLevel t =
    case findInfixOp mulOps t of
        Just (lhs, op, rhs) -> SEPrim (normalizeOp op) (parseAtom lhs) (parseAtom rhs)
        Nothing -> parseAtom t
  where
    mulOps = ["*", "/", "%"]

{- | Find a top-level infix operator in the text.
Scans right-to-left for left-associativity.
Returns (lhs, op, rhs) or Nothing.
-}
findInfixOp :: [Text] -> Text -> Maybe (Text, Text, Text)
findInfixOp ops t = go (T.length t - 1) 0
  where
    go :: Int -> Int -> Maybe (Text, Text, Text)
    go pos _
        | pos < 1 = Nothing -- need at least 1 char on lhs
    go pos depth =
        let c = T.index t pos
            depth' = case c of
                ')' -> depth + 1
                '(' -> depth - 1
                ']' -> depth + 1
                '[' -> depth - 1
                '"' -> depth -- simplified: don't track strings r-to-l
                _ -> depth
         in if depth' == 0
                then case findMatchingOp ops t pos of
                    Just (op, opStart) ->
                        let lhs = T.strip (T.take opStart t)
                            rhs = T.strip (T.drop (opStart + T.length op) t)
                         in if not (T.null lhs)
                                && not (T.null rhs)
                                -- Make sure the op is surrounded by spaces (not part of identifier)
                                && (opStart == 0 || isSpace (T.index t (opStart - 1)))
                                then Just (lhs, op, rhs)
                                else go (pos - 1) depth'
                    Nothing -> go (pos - 1) depth'
                else go (pos - 1) depth'

-- | Check if any operator matches at the given position (right-to-left).
findMatchingOp :: [Text] -> Text -> Int -> Maybe (Text, Int)
findMatchingOp [] _ _ = Nothing
findMatchingOp (op : rest) t pos =
    let opLen = T.length op
        start = pos - opLen + 1
     in if start >= 0
            && T.take opLen (T.drop start t) == op
            -- Ensure the char after the op is a space (or end of string)
            && (start + opLen >= T.length t || isSpace (T.index t (start + opLen)))
            then Just (op, start)
            else findMatchingOp rest t pos

-- | Parse an atomic expression (no infix operators at this level).
parseAtom :: Text -> ScalaExpr
parseAtom t
    | T.null s = SEUnparsed ""
    -- if/then/else
    | T.isPrefixOf "if " s = parseIfExpr s
    -- String literal
    | T.isPrefixOf "\"" s = SELitStr (parseStringLit s)
    -- Integer literal
    | isIntLiteral s = SELitInt (read (T.unpack s))
    -- new Constructor(args)
    | T.isPrefixOf "new " s = parseNewExpr s
    -- val binding
    | T.isPrefixOf "val " s = parseValExpr s
    -- Block expression
    | T.isPrefixOf "{" s = parseBlockExpr s
    -- Parenthesized expression
    | T.isPrefixOf "(" s = parseParenExpr s
    -- Method call: expr.method(args) or Name.method(args)
    | hasDotCall s = parseDotExpr s
    -- Simple function call: name(args)
    | hasParenCall s = parseCallExpr s
    -- Variable reference
    | isIdentifier s = SEVar s
    -- Fallback
    | otherwise = SEUnparsed s
  where
    s = T.strip t

-- | Parse a parenthesized expression: (expr)
parseParenExpr :: Text -> ScalaExpr
parseParenExpr t =
    case T.stripPrefix "(" t of
        Just rest ->
            let (inner, _) = findCloseParen rest 1
             in parseExpr inner
        Nothing -> SEUnparsed t

-- | Check if text looks like an integer literal.
isIntLiteral :: Text -> Bool
isIntLiteral t =
    case T.uncons t of
        Just ('-', rest) -> not (T.null rest) && T.all isDigit rest
        Just (c, rest) -> isDigit c && T.all isDigit rest
        Nothing -> False

-- | Check if text is a simple identifier.
isIdentifier :: Text -> Bool
isIdentifier t =
    not (T.null t)
        && T.all (\c -> isAlphaNum c || c == '_' || c == '$' || c == '.') t

-- | Check if text contains a dot-call pattern.
hasDotCall :: Text -> Bool
hasDotCall t =
    -- Look for patterns like: name.method( or name.op(
    let (before, after) = findTopLevelDot t
     in not (T.null before) && not (T.null after)

-- | Find the first top-level dot (not inside parens/brackets).
findTopLevelDot :: Text -> (Text, Text)
findTopLevelDot = go 0 T.empty
  where
    go :: Int -> Text -> Text -> (Text, Text)
    go _ acc t
        | T.null t = (acc, T.empty)
    go depth acc t =
        case T.uncons t of
            Nothing -> (acc, T.empty)
            Just ('(', rest) -> go (depth + 1) (T.snoc acc '(') rest
            Just (')', rest) -> go (max 0 (depth - 1)) (T.snoc acc ')') rest
            Just ('[', rest) -> go (depth + 1) (T.snoc acc '[') rest
            Just (']', rest) -> go (max 0 (depth - 1)) (T.snoc acc ']') rest
            Just ('"', rest) ->
                let (str, afterStr) = T.break (== '"') rest
                 in case T.uncons afterStr of
                        Just (_, r) -> go depth (acc <> "\"" <> str <> "\"") r
                        Nothing -> (acc <> "\"" <> str, T.empty)
            Just ('.', rest)
                | depth == 0 && not (T.null acc) && not (T.null rest) ->
                    (acc, rest)
            Just (c, rest) -> go depth (T.snoc acc c) rest

-- | Check if text has a simple parenthesized call.
hasParenCall :: Text -> Bool
hasParenCall t =
    case T.breakOn "(" t of
        (before, after) ->
            not (T.null before)
                && not (T.null after)
                && T.all (\c -> isAlphaNum c || c == '_' || c == '$' || c == '.') (T.strip before)

-- | Parse an if/then/else expression.
parseIfExpr :: Text -> ScalaExpr
parseIfExpr t =
    let afterIf = T.strip $ T.drop 3 t -- skip "if "
    -- Find "then" keyword at top level
        (condText, rest1) = findKeyword "then" afterIf
        condExpr = parseExpr condText
        -- Find "else" keyword at top level
        (thenText, elseText) = findKeyword "else" (T.strip rest1)
        thenExpr = parseExpr thenText
        elseExpr = parseExpr elseText
     in SEIf condExpr thenExpr elseExpr

-- | Find a keyword at the top level (not inside parens).
findKeyword :: Text -> Text -> (Text, Text)
findKeyword kw = go 0 T.empty
  where
    kwLen = T.length kw
    go :: Int -> Text -> Text -> (Text, Text)
    go _ acc t
        | T.null t = (acc, T.empty)
    go depth acc t
        | depth == 0 && T.isPrefixOf kw t =
            let after = T.drop kwLen t
             in if T.null after || isSpace (T.head after)
                    then (T.strip acc, T.strip after)
                    else advance depth acc t
        | otherwise = advance depth acc t
    advance depth acc t =
        case T.uncons t of
            Nothing -> (acc, T.empty)
            Just ('(', rest) -> go (depth + 1) (T.snoc acc '(') rest
            Just (')', rest) -> go (max 0 (depth - 1)) (T.snoc acc ')') rest
            Just (c, rest) -> go depth (T.snoc acc c) rest

-- | Parse a string literal, handling basic escapes.
parseStringLit :: Text -> Text
parseStringLit t =
    let inner = T.drop 1 t -- skip opening quote
        content = T.takeWhile (/= '"') inner
     in content

-- | Parse a "new Constructor(args)" expression.
parseNewExpr :: Text -> ScalaExpr
parseNewExpr t =
    let afterNew = T.strip $ T.drop 4 t -- skip "new "
        name = T.takeWhile (\c -> isAlphaNum c || c == '_' || c == '.' || c == '$') afterNew
        afterName = T.drop (T.length name) afterNew
     in case T.stripPrefix "(" (T.strip afterName) of
            Just argsStr ->
                let (inner, _) = findCloseParen argsStr 1
                    args = if T.null (T.strip inner) then [] else map (parseExpr . T.strip) (splitOnComma inner)
                 in SENew name args
            Nothing -> SENew name []

-- | Parse a val binding: "val x = expr; rest" or block-level val.
parseValExpr :: Text -> ScalaExpr
parseValExpr t =
    let afterVal = T.strip $ T.drop 4 t -- skip "val "
    -- name: Type = expr  or  name = expr
        (nameAndType, rest) = T.breakOn "= " afterVal
        name = T.strip $ case T.breakOn ":" nameAndType of
            (n, _) -> T.strip n
        bodyText = T.drop 2 rest
     in SELet name (parseExpr bodyText) (SEUnparsed "")

-- | Parse a block expression { ... }.
parseBlockExpr :: Text -> ScalaExpr
parseBlockExpr t =
    let inner = T.strip $ T.drop 1 t -- skip "{"
    -- Find matching close brace
        content = T.strip $ T.dropEnd 1 $ fst $ findCloseBrace inner 1
     in if T.null content then SEUnparsed "{}" else parseExpr content

findCloseBrace :: Text -> Int -> (Text, Text)
findCloseBrace t depth
    | T.null t = (T.empty, T.empty)
    | depth == 0 = (T.empty, t)
    | otherwise =
        case T.uncons t of
            Nothing -> (T.empty, T.empty)
            Just ('{', rest) ->
                let (inner, remaining) = findCloseBrace rest (depth + 1)
                 in (T.cons '{' inner, remaining)
            Just ('}', rest)
                | depth == 1 -> (T.empty, rest)
                | otherwise ->
                    let (inner, remaining) = findCloseBrace rest (depth - 1)
                     in (T.cons '}' inner, remaining)
            Just (c, rest) ->
                let (inner, remaining) = findCloseBrace rest depth
                 in (T.cons c inner, remaining)

{- | Parse a dot-call expression: receiver.method(args)
Handles: n.*(m), n.-(1), n.<=(1), ScalaTest.factorial(n.-(1))
Also handles boxing: Int.box(expr) -> strip the box wrapper.
-}
parseDotExpr :: Text -> ScalaExpr
parseDotExpr t =
    let (receiver, methodAndArgs) = findTopLevelDot t
        -- methodAndArgs is "method(args)" or "op(args)"
        method = T.takeWhile (/= '(') methodAndArgs
        afterMethod = T.drop (T.length method) methodAndArgs
     in if isArithOp method
            then -- Binary primitive operator via dot notation: n.*(m)
                case T.stripPrefix "(" (T.strip afterMethod) of
                    Just argsStr ->
                        let (inner, _) = findCloseParen argsStr 1
                         in SEPrim (normalizeOp method) (parseExpr receiver) (parseExpr inner)
                    Nothing -> SEApp (SEVar (receiver <> "." <> method)) []
            -- Boxing: Int.box(expr), Long.box(expr) etc. -> unwrap
            else
                if method == "box" && isBoxableType receiver
                    then case T.stripPrefix "(" (T.strip afterMethod) of
                        Just argsStr ->
                            let (inner, _) = findCloseParen argsStr 1
                             in parseExpr inner -- strip the boxing wrapper
                        Nothing -> SEVar (receiver <> "." <> method)
                    -- Qualified function call: Module.function(args) where receiver is a simple name
                    else
                        if isIdentifier receiver
                            then case T.stripPrefix "(" (T.strip afterMethod) of
                                Just argsStr ->
                                    let qname = receiver <> "." <> method
                                        (inner, _) = findCloseParen argsStr 1
                                        args =
                                            if T.null (T.strip inner)
                                                then []
                                                else map (parseExpr . T.strip) (splitOnComma inner)
                                     in SEApp (SEVar qname) args
                                Nothing ->
                                    -- Qualified name like Module.name (no args)
                                    SEVar (receiver <> "." <> method)
                            else -- Method call on a complex receiver
                                let rcv = parseExpr receiver
                                 in case T.stripPrefix "(" (T.strip afterMethod) of
                                        Just argsStr ->
                                            let (inner, _) = findCloseParen argsStr 1
                                                args =
                                                    if T.null (T.strip inner)
                                                        then []
                                                        else map (parseExpr . T.strip) (splitOnComma inner)
                                             in SEApp (SEVar method) (rcv : args)
                                        Nothing ->
                                            SEVar (receiver <> "." <> method)

-- | Check if a type name is a boxable primitive.
isBoxableType :: Text -> Bool
isBoxableType t =
    t
        `elem` ["Int", "Long", "Double", "Float", "Boolean", "Char", "Byte", "Short"]

-- | Check if a method name is an arithmetic/comparison operator.
isArithOp :: Text -> Bool
isArithOp op =
    op
        `elem` [ "+"
               , "-"
               , "*"
               , "/"
               , "%"
               , "<="
               , ">="
               , "<"
               , ">"
               , "=="
               , "!="
               , "&"
               , "|"
               , "^"
               , "<<"
               , ">>"
               , ">>>"
               ]

-- | Normalize a Scala operator name to OrganIR primitive name.
normalizeOp :: Text -> Text
normalizeOp "+" = "add"
normalizeOp "-" = "sub"
normalizeOp "*" = "mul"
normalizeOp "/" = "div"
normalizeOp "%" = "mod"
normalizeOp "<=" = "le"
normalizeOp ">=" = "ge"
normalizeOp "<" = "lt"
normalizeOp ">" = "gt"
normalizeOp "==" = "eq"
normalizeOp "!=" = "ne"
normalizeOp "&" = "and"
normalizeOp "|" = "or"
normalizeOp "^" = "xor"
normalizeOp "<<" = "shl"
normalizeOp ">>" = "shr"
normalizeOp ">>>" = "ushr"
normalizeOp op = op

-- | Parse a simple function call: name(args)
parseCallExpr :: Text -> ScalaExpr
parseCallExpr t =
    let (name, rest) = T.breakOn "(" t
     in case T.stripPrefix "(" rest of
            Just argsStr ->
                let (inner, _) = findCloseParen argsStr 1
                    cleanName = T.strip name
                    args =
                        if T.null (T.strip inner)
                            then []
                            else map (parseExpr . T.strip) (splitOnComma inner)
                 in -- Check for boxing calls like Int.box(expr)
                    if T.isSuffixOf ".box" cleanName
                        then case args of
                            [arg] -> arg -- strip boxing
                            _ -> SEApp (SEVar cleanName) args
                        else SEApp (SEVar cleanName) args
            Nothing -> SEVar (T.strip t)

-- ---------------------------------------------------------------------------
-- OrganIR Emission via organ-ir library
-- ---------------------------------------------------------------------------

-- | Emit the parsed Scala classes as OrganIR JSON.
emitOrganIR :: String -> FilePath -> [ScalaClass] -> Text
emitOrganIR modName srcFile classes =
    let allDefs = concatMap scDefs classes
        userDefs = filter (not . isCompilerGenerated) allDefs
        irDefs = zipWith (defToIR (T.pack modName)) [1 ..] userDefs
     in renderOrganIR $
            IR.simpleOrganIR IR.LScala3 "scala3-shim-0.1" (T.pack modName) srcFile irDefs

-- | Check if a definition is compiler-generated boilerplate.
isCompilerGenerated :: ScalaDef -> Bool
isCompilerGenerated d =
    sdName d `elem` ["writeReplace", "$init$", "<init>", "<clinit>"]

-- | Translate a ScalaDef to an OrganIR Definition.
defToIR :: Text -> Int -> ScalaDef -> IR.Definition
defToIR modName uid def =
    let n = sdName def
        params = sdParams def
        retTy = sdReturnType def
        vis = if sdVisibility def == "public" then IR.Public else IR.Private
        argTys = map (\(_, ty) -> IR.FnArg (Just IR.Many) (mapScalaTypeToIR ty)) params
        eff =
            if n == "main"
                then IR.EffectRow [IR.qualName "std" "io"] Nothing
                else IR.pureEffect
        fnTy = IR.TFn argTys eff (mapScalaTypeToIR retTy)
        bodyExpr = exprToIR (sdBody def)
        lamExpr = mkLam params bodyExpr
     in IR.Definition
            { IR.defName = IR.QName modName (IR.Name n uid)
            , IR.defType = fnTy
            , IR.defExpr = lamExpr
            , IR.defSort = IR.SFun
            , IR.defVisibility = vis
            , IR.defArity = length params
            }

-- | Wrap an expression in a lambda with typed parameters.
mkLam :: [(Text, Text)] -> IR.Expr -> IR.Expr
mkLam [] body = body
mkLam params body =
    IR.ELam (zipWith mkLamParam [100 ..] params) body

mkLamParam :: Int -> (Text, Text) -> IR.LamParam
mkLamParam uid (n, ty) =
    IR.LamParam (IR.Name n uid) (Just (mapScalaTypeToIR ty))

-- | Map a Scala type name to an OrganIR type.
mapScalaTypeToIR :: Text -> IR.Ty
mapScalaTypeToIR = IR.TCon . IR.qualName "scala" . mapScalaType

-- | Map Scala types to OrganIR type names.
mapScalaType :: Text -> Text
mapScalaType "Int" = "int"
mapScalaType "Long" = "long"
mapScalaType "Double" = "double"
mapScalaType "Float" = "float"
mapScalaType "Boolean" = "bool"
mapScalaType "String" = "string"
mapScalaType "Unit" = "unit"
mapScalaType "Char" = "char"
mapScalaType "Byte" = "byte"
mapScalaType "Short" = "short"
mapScalaType "Any" = "any"
mapScalaType "Object" = "any"
mapScalaType "Nothing" = "nothing"
mapScalaType t
    | T.isSuffixOf "[]" t = "array[" <> mapScalaType (T.dropEnd 2 t) <> "]"
    | otherwise = t

-- | Translate a ScalaExpr to an OrganIR Expr.
exprToIR :: ScalaExpr -> IR.Expr
exprToIR (SELitInt n) = IR.ELit (IR.LitInt (fromIntegral n))
exprToIR (SELitStr s) = IR.ELit (IR.LitString s)
exprToIR (SEVar v) = varToIR v
exprToIR (SEApp fn args) = IR.EApp (exprToIR fn) (map exprToIR args)
exprToIR (SEPrim op lhs rhs) = IR.EApp (IR.eVar op) [exprToIR lhs, exprToIR rhs]
exprToIR (SEIf cond thenE elseE) = IR.eIf (exprToIR cond) (exprToIR thenE) (exprToIR elseE)
exprToIR (SELet n bound body) = IR.ELet [IR.LetBind (IR.name n) Nothing (exprToIR bound)] (exprToIR body)
exprToIR (SENew n args) = IR.ECon (IR.localName n) (map exprToIR args)
exprToIR (SEBlock es) = IR.eSeq (map exprToIR es)
exprToIR (SEUnparsed t)
    | T.null t = IR.EApp (IR.eVar "unparsed") [IR.eString "<empty>"]
    | otherwise = IR.EApp (IR.eVar "unparsed") [IR.eString t]

-- | Translate a variable name, handling qualified names like "Module.name".
varToIR :: Text -> IR.Expr
varToIR v =
    case T.breakOnEnd "." v of
        (prefix, localN)
            | T.null prefix -> IR.EVar (IR.name v)
            | otherwise -> IR.EVar (IR.Name localN 0)
