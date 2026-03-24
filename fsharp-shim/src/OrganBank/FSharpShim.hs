{- | F# Typed AST Extraction Shim

Invokes @dotnet fsi --typedtree --typedtreetypes@ on an F# source file
to dump the typed intermediate representation (to stderr), then parses
the indentation-based output and emits OrganIR JSON.

The typed tree preserves type information, IL primitives (@#AI_mul#@ etc.),
and post-optimisation structure.
-}
module OrganBank.FSharpShim (
    extractOrganIR,
) where

import Data.Char (isAlphaNum, isAsciiLower, isDigit, isSpace)
import Data.List (isPrefixOf, isSuffixOf, stripPrefix, unsnoc)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, takeExtension)
import System.Process (readProcessWithExitCode)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Extract F# typed AST from a @.fsx@ or @.fs@ file and return OrganIR JSON.
extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
    let ext = takeExtension inputPath
        args = case ext of
            ".fsx" -> ["--typedtree", "--typedtreetypes", inputPath]
            _ -> ["--typedtree", "--typedtreetypes", "--use:" ++ inputPath]
    (exitCode, _stdout, stderr_) <-
        readProcessWithExitCode "dotnet" ("fsi" : args) ""
    case exitCode of
        ExitSuccess -> processOutput inputPath stderr_
        ExitFailure _ ->
            -- F# sometimes exits non-zero but still dumps the tree
            if hasPassEnd stderr_
                then processOutput inputPath stderr_
                else pure (Left $ "dotnet fsi failed:\n" <> stderr_)

processOutput :: FilePath -> String -> IO (Either String Text)
processOutput inputPath stderr_ = do
    let modName = capitalise (takeBaseName inputPath)
        passEnd = extractPassEnd stderr_
        defs = parseTypedTree passEnd
        json = emitOrganIR modName inputPath defs
    pure (Right json)

hasPassEnd :: String -> Bool
hasPassEnd s = "pass-end" `isInfixOf'` s
  where
    isInfixOf' needle haystack = any (isPrefixOf needle) (tails' haystack)
    tails' [] = [[]]
    tails' xs@(_ : rest) = xs : tails' rest

-- ---------------------------------------------------------------------------
-- Typed tree extraction
-- ---------------------------------------------------------------------------

{- | Extract the pass-end section from the full stderr output.
The pass-end section starts after a line containing "pass-end" and
continues until the next dashed-line delimiter or end of input.
-}
extractPassEnd :: String -> String
extractPassEnd input =
    let ls = lines input
        -- Find the pass-end section
        afterPassEnd = dropWhile (not . isPassEndLine) ls
     in case afterPassEnd of
            [] -> input -- Fallback: use entire input
            (_ : rest) ->
                -- Take lines until the next delimiter or end
                let content = takeWhile (not . isDashLine) rest
                 in unlines content

isPassEndLine :: String -> Bool
isPassEndLine l = "pass-end" `isPrefixOf` strip l

isDashLine :: String -> Bool
isDashLine l =
    let s = strip l
     in length s > 3 && all (== '-') s

-- ---------------------------------------------------------------------------
-- Typed tree types
-- ---------------------------------------------------------------------------

data FSDef = FSDef
    { fdName :: String
    , fdParams :: [FSParam]
    , fdRetTy :: String
    , fdBody :: FSExpr
    }
    deriving (Show)

data FSParam = FSParam
    { fpName :: String
    , fpType :: String
    }
    deriving (Show)

data FSExpr
    = FSVar String
    | FSIntLit Int
    | FSStrLit String
    | FSApp FSExpr [FSExpr]
    | FSLam [FSParam] FSExpr
    | FSLet String FSExpr FSExpr
    | FSIf FSExpr FSExpr FSExpr
    | -- | IL intrinsic
      FSPrim String [FSExpr]
    | -- | Const(value, type)
      FSConst String String
    | -- | Unparsed fallback
      FSRaw String
    deriving (Show)

-- ---------------------------------------------------------------------------
-- Typed tree parser
-- ---------------------------------------------------------------------------

{- | Parse the typed tree text into definitions.
We look for top-level @let@ bindings at the outermost indentation level
within the typedImplFile structure.
-}
parseTypedTree :: String -> [FSDef]
parseTypedTree input =
    let ls = lines input
     in extractDefs ls

-- | Extract top-level definitions by scanning for let-binding lines.
extractDefs :: [String] -> [FSDef]
extractDefs [] = []
extractDefs (l : ls)
    | isLetBinding (strip l) =
        let (binding, rest) = spanBlock l ls
            def = parseBinding binding
         in def ++ extractDefs rest
    | otherwise = extractDefs ls

-- | Check if a line starts a let binding: "let name ..." or "let rec name ..."
isLetBinding :: String -> Bool
isLetBinding s =
    "let " `isPrefixOf` s
        && not ("letBinding" `isPrefixOf` s)
        && not ("[let" `isPrefixOf` s)

-- | Collect a binding and its indented body lines.
spanBlock :: String -> [String] -> (String, [String])
spanBlock header rest =
    let indent = countIndent header
        (bodyLines, remaining) = span (isMoreIndentedOrBlank indent) rest
     in (unlines (header : bodyLines), remaining)

isMoreIndentedOrBlank :: Int -> String -> Bool
isMoreIndentedOrBlank baseIndent l
    | all isSpace l = True
    | otherwise = countIndent l > baseIndent

countIndent :: String -> Int
countIndent = length . takeWhile isSpace

-- | Parse a single let binding block into a definition.
parseBinding :: String -> [FSDef]
parseBinding block =
    let ls = lines block
     in case ls of
            [] -> []
            (header : bodyLines) ->
                let stripped = strip header
                    -- Parse: let [rec] name (params) : retTy =
                    (name, params, retTy) = parseLetHeader stripped
                    bodyText = unlines bodyLines
                    body = parseExpr (strip bodyText)
                 in [FSDef name params retTy body]

{- | Parse a let-binding header line.
Forms: "let name (p1: t1) (p2: t2) : retTy ="
       "let rec name (p1: t1) : retTy ="
       "let name : type ="
-}
parseLetHeader :: String -> (String, [FSParam], String)
parseLetHeader s =
    let
        -- Strip "let " or "let rec "
        s1 = case stripPrefix "let rec " s of
            Just r1 -> r1
            Nothing -> fromMaybe s (stripPrefix "let " s)
        -- Extract name (up to first space, paren, or colon)
        (name, rest) = span (\c -> isAlphaNum c || c == '_' || c == '\'') s1
        -- Parse parameters and return type from the rest
        (params, retTy) = parseParamsAndRet (strip rest)
     in
        (name, params, retTy)

{- | Parse parameters and return type from the remainder after the name.
"(n: int) : int =" -> ([FSParam "n" "int"], "int")
-}
parseParamsAndRet :: String -> ([FSParam], String)
parseParamsAndRet s = go s []
  where
    go ('(' : rest) acc =
        -- Parse a parameter: "name: type)"
        let (paramStr, after) = break (== ')') rest
            (pName, pType) = parseParamStr paramStr
            rest' = case after of
                (')' : r) -> strip r
                _ -> strip after
         in go rest' (acc ++ [FSParam pName pType])
    go (':' : rest) acc =
        -- Return type follows
        let retTy = strip (takeWhile (/= '=') rest)
         in (acc, retTy)
    go ('=' : _) acc = (acc, "any")
    go [] acc = (acc, "any")
    go (_ : rest) acc = go rest acc

parseParamStr :: String -> (String, String)
parseParamStr s =
    let s' = strip s
     in case break (== ':') s' of
            (name, ':' : ty) -> (strip name, strip ty)
            (name, _) -> (strip name, "any")

{- | Parse an expression from the typed tree text.
This is necessarily approximate since the typed tree is not a formal grammar.
-}
parseExpr :: String -> FSExpr
parseExpr s
    | null s = FSRaw ""
    -- IL intrinsics: #AI_mul#(a, b) or similar
    | "#AI_" `isPrefixOf` s = parseIntrinsic s
    -- if-then-else
    | "if " `isPrefixOf` s || "if(" `isPrefixOf` s = parseIfExpr s
    -- Const(value, type)
    | "Const" `isPrefixOf` s = parseConst s
    -- Integer literal
    | all isDigit s && not (null s) = FSIntLit (read s)
    | isNegInt s = FSIntLit (read s)
    -- String literal
    | "\"" `isPrefixOf` s = FSStrLit (unquoteStr s)
    -- Let binding inside expression
    | "let " `isPrefixOf` s = parseLetExpr s
    -- Lambda
    | "fun " `isPrefixOf` s || "(fun " `isPrefixOf` s = parseLambdaExpr s
    -- Application: name(args) or name arg
    | hasApplication s = parseApplication s
    -- Switch
    | "Switch" `isPrefixOf` s || "switch" `isPrefixOf` s = parseSwitch s
    -- Variable reference
    | isVarLike s = FSVar (cleanVar s)
    -- Fallback
    | otherwise = FSRaw s
  where
    isNegInt ('-' : cs) = not (null cs) && all isDigit cs
    isNegInt _ = False

-- | Parse an IL intrinsic call: #AI_mul#(a, b)
parseIntrinsic :: String -> FSExpr
parseIntrinsic s =
    let
        -- Extract the intrinsic name between # delimiters
        (intrName, rest) = spanIntrinsic s
        primName = translateIntrinsic intrName
        args = parseIntrinsicArgs (strip rest)
     in
        FSPrim primName args

spanIntrinsic :: String -> (String, String)
spanIntrinsic ('#' : rest) =
    let (name, after) = break (== '#') rest
     in case after of
            ('#' : r) -> (name, r)
            _ -> (name, after)
spanIntrinsic s = break isSpace s

translateIntrinsic :: String -> String
translateIntrinsic s = case s of
    "AI_mul" -> "mul"
    "AI_sub" -> "sub"
    "AI_add" -> "add"
    "AI_ceq" -> "eq"
    "AI_cgt" -> "gt"
    "AI_clt" -> "lt"
    "AI_neg" -> "neg"
    "AI_div" -> "div"
    "AI_rem" -> "rem"
    "AI_shl" -> "shl"
    "AI_shr" -> "shr"
    "AI_and" -> "and"
    "AI_or" -> "or"
    "AI_xor" -> "xor"
    "AI_conv" -> "conv"
    _ -> s -- pass through unknown intrinsics

parseIntrinsicArgs :: String -> [FSExpr]
parseIntrinsicArgs ('(' : rest) =
    let inner = takeWhile (/= ')') rest
     in map (parseExpr . strip) (splitComma inner)
parseIntrinsicArgs s =
    -- Sometimes args follow without parens, separated by spaces
    case words s of
        [] -> []
        ws -> map (parseExpr . strip) (take 2 ws)

-- | Split on commas, respecting parenthesised nesting.
splitComma :: String -> [String]
splitComma = go (0 :: Int) ""
  where
    go _ acc [] = [reverse acc]
    go 0 acc (',' : rest) = reverse acc : go 0 "" (dropWhile isSpace rest)
    go d acc ('(' : rest) = go (d + 1) ('(' : acc) rest
    go d acc (')' : rest) = go (max 0 (d - 1)) (')' : acc) rest
    go d acc (c : rest) = go d (c : acc) rest

-- | Parse if-then-else expression.
parseIfExpr :: String -> FSExpr
parseIfExpr s =
    let s' = case stripPrefix "if " s of
            Just rest -> rest
            Nothing -> case stripPrefix "if(" s of
                Just rest -> "(" ++ rest
                Nothing -> s
        -- Find "then" and "else" keywords at appropriate nesting
        (condStr, thenElse) = splitAtKeyword "then" s'
        (thenStr, elseStr) = splitAtKeyword "else" thenElse
     in FSIf
            (parseExpr (strip condStr))
            (parseExpr (strip thenStr))
            (parseExpr (strip elseStr))

-- | Split a string at a keyword, respecting parenthesised nesting.
splitAtKeyword :: String -> String -> (String, String)
splitAtKeyword kw = go (0 :: Int) ""
  where
    kwLen = length kw
    go _ acc [] = (reverse acc, "")
    go 0 acc rest
        | kw `isPrefixOf` rest
        , let after = drop kwLen rest
        , case after of [] -> True; (c' : _) -> isSpace c' =
            (reverse acc, strip (drop kwLen rest))
    go d acc ('(' : rest) = go (d + 1) ('(' : acc) rest
    go d acc (')' : rest) = go (max 0 (d - 1)) (')' : acc) rest
    go d acc (c : rest) = go d (c : acc) rest

-- | Parse Const(value, type)
parseConst :: String -> FSExpr
parseConst s =
    let inner = maybe s strip (stripPrefix "Const" s)
        -- Remove parens
        content = case inner of
            ('(' : rest) -> takeWhile (/= ')') rest
            _ -> inner
        parts = splitComma content
     in case parts of
            (val : _) ->
                let v = strip val
                 in if (all isDigit v && not (null v)) || isNegIntStr v
                        then FSIntLit (read v)
                        else FSStrLit v
            [] -> FSRaw s
  where
    isNegIntStr ('-' : cs) = not (null cs) && all isDigit cs
    isNegIntStr _ = False

-- | Parse a let expression inside a body.
parseLetExpr :: String -> FSExpr
parseLetExpr s =
    let s' = fromMaybe s (stripPrefix "let " s)
        -- Split at "in" keyword
        (bindingStr, bodyStr) = splitAtKeyword "in" s'
        -- Parse the binding: "name = expr"
        (bname, bexpr) = case break (== '=') bindingStr of
            (n, '=' : e) -> (strip n, parseExpr (strip e))
            _ -> (strip bindingStr, FSRaw "")
        body = parseExpr (strip bodyStr)
     in FSLet (cleanVar bname) bexpr body

-- | Parse a lambda expression.
parseLambdaExpr :: String -> FSExpr
parseLambdaExpr s =
    let s' = stripParens s
        rest = fromMaybe s' (stripPrefix "fun " s')
        -- Parse parameters until ->
        (paramStr, bodyStr) = splitAtArrow rest
        params = parseParams paramStr
        body = parseExpr (strip bodyStr)
     in FSLam params body

splitAtArrow :: String -> (String, String)
splitAtArrow = go ""
  where
    go acc [] = (reverse acc, "")
    go acc ('-' : '>' : rest) = (reverse acc, rest)
    go acc (c : rest) = go (c : acc) rest

parseParams :: String -> [FSParam]
parseParams s =
    let parts = words s
     in mapParams parts
  where
    mapParams [] = []
    mapParams (('(' : rest) : more) =
        -- "(name: type)" possibly split across words
        let full = unwords (('(' : rest) : more)
            inner = takeWhile (/= ')') (drop 1 full)
            (pn, pt) = parseParamStr inner
         in [FSParam pn pt]
    mapParams (w : ws) = FSParam (cleanVar w) "any" : mapParams ws

-- | Parse an application: "f(a, b)" or "f a b"
parseApplication :: String -> FSExpr
parseApplication s
    | '(' `elem` s =
        let (fn, rest) = break (== '(') s
         in if null fn
                then FSRaw s
                else
                    let argsStr = takeWhile (/= ')') (drop 1 rest)
                        args = map (parseExpr . strip) (splitComma argsStr)
                     in FSApp (parseExpr (strip fn)) args
    | otherwise =
        let ws = words s
         in case ws of
                [] -> FSRaw s
                [x] -> FSVar (cleanVar x)
                (f : as) -> FSApp (FSVar (cleanVar f)) (map (parseExpr . strip) as)

hasApplication :: String -> Bool
hasApplication s =
    let ws = words s
     in length ws > 1 || ('(' `elem` s && not ("(" `isPrefixOf` s && ")" `isSuffixOf` s))

-- | Parse a switch/match expression (best effort).
parseSwitch :: String -> FSExpr
parseSwitch s =
    let
        -- Try to extract the scrutinee and produce a case
        rest = maybe (maybe s strip (stripPrefix "switch" s)) strip (stripPrefix "Switch" s)
        scrut = parseExpr (takeWhile (\c -> c /= '{' && c /= '\n') rest)
     in
        FSIf scrut (FSRaw "branch_true") (FSRaw "branch_false")

isVarLike :: String -> Bool
isVarLike [] = False
isVarLike s = all (\c -> isAlphaNum c || c == '_' || c == '\'' || c == '.') s

cleanVar :: String -> String
cleanVar = filter (\c -> isAlphaNum c || c == '_' || c == '\'' || c == '.')

stripParens :: String -> String
stripParens ('(' : s) = case unsnoc s of
    Just (inner, ')') -> inner
    _ -> '(' : s
stripParens s = s

unquoteStr :: String -> String
unquoteStr ('"' : rest) = case reverse rest of
    ('"' : r) -> reverse r
    _ -> rest
unquoteStr s = s

-- ---------------------------------------------------------------------------
-- OrganIR emission via organ-ir library
-- ---------------------------------------------------------------------------

emitOrganIR :: String -> FilePath -> [FSDef] -> Text
emitOrganIR modName srcFile defs =
    let metadata =
            IR.Metadata IR.LFSharp Nothing (Just (T.pack srcFile)) "fsharp-shim-0.1" Nothing
        exports = map (T.pack . fdName) defs
        irDefs = zipWith (translateDef (T.pack modName)) [0 ..] defs
        module_ =
            IR.Module (T.pack modName) exports irDefs [] []
     in renderOrganIR $ IR.OrganIR metadata module_

translateDef :: Text -> Int -> FSDef -> IR.Definition
translateDef modName uniq def =
    let nameText = T.pack (fdName def)
        qname = IR.QName modName (IR.Name nameText uniq)
        params = fdParams def
        arity = length params
        sort_ = if arity > 0 then IR.SFun else IR.SVal
        ty =
            if arity > 0
                then translateFnType params (fdRetTy def)
                else translateTypeRef (fdRetTy def)
        expr = translateExpr (fdBody def)
     in IR.Definition qname ty expr sort_ IR.Public arity

translateFnType :: [FSParam] -> String -> IR.Ty
translateFnType params retTy =
    IR.TFn
        (map (IR.FnArg (Just IR.Many) . translateTypeRef . fpType) params)
        IR.pureEffect
        (translateTypeRef retTy)

translateTypeRef :: String -> IR.Ty
translateTypeRef "int" = IR.tCon "int"
translateTypeRef "string" = IR.tCon "string"
translateTypeRef "bool" = IR.tCon "bool"
translateTypeRef "float" = IR.tCon "float"
translateTypeRef "unit" = IR.tCon "unit"
translateTypeRef _ = IR.TAny

translateExpr :: FSExpr -> IR.Expr
translateExpr (FSVar n) = IR.eVar (T.pack n)
translateExpr (FSIntLit n) = IR.eInt (fromIntegral n)
translateExpr (FSStrLit s) = IR.eString (T.pack s)
translateExpr (FSApp fn args) = IR.EApp (translateExpr fn) (map translateExpr args)
translateExpr (FSLam params body) =
    IR.ELam
        (map (\p -> IR.LamParam (IR.name (T.pack (fpName p))) (Just (translateTypeRef (fpType p)))) params)
        (translateExpr body)
translateExpr (FSLet n bind body) =
    IR.ELet [IR.LetBind (IR.name (T.pack n)) Nothing (translateExpr bind)] (translateExpr body)
translateExpr (FSIf cond thn els) = IR.eIf (translateExpr cond) (translateExpr thn) (translateExpr els)
translateExpr (FSPrim n args) = IR.EApp (IR.eVar (T.pack n)) (map translateExpr args)
translateExpr (FSConst val ty) = IR.eString (T.pack (val ++ ":" ++ ty))
translateExpr (FSRaw s) = IR.eString (T.pack s)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

capitalise :: String -> String
capitalise [] = []
capitalise (c : cs) = toUpper' c : cs
  where
    toUpper' x
        | isAsciiLower x = toEnum (fromEnum x - 32)
        | otherwise = x
