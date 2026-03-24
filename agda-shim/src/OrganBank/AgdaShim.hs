{- | Agda Treeless IR Extraction Shim

Runs @agda --compile --ghc-dont-call-ghc@ on an Agda source file,
which produces MAlonzo-generated Haskell files under @MAlonzo/Code/@.
These generated files closely mirror Agda's internal Treeless IR:
case splits, let bindings, primitive ops, constructor applications.

We parse the generated Haskell to extract definitions, then emit
OrganIR JSON on stdout. The MAlonzo directory is cleaned up afterward.
-}
module OrganBank.AgdaShim (
    extractOrganIR,
) where

import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Directory (doesDirectoryExist, listDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | A parsed MAlonzo function definition.
data MAlonzoDef = MAlonzoDef
    { mdName :: String
    -- ^ Cleaned function name
    , mdParams :: [String]
    -- ^ Parameter names
    , mdBody :: String
    -- ^ Raw body text
    }
    deriving (Show)

-- | OrganIR expression tree.
data Expr
    = EVar String Int
    | EApp String Int [Expr]
    | ELet String Int Expr Expr
    | ECase String Int [Branch]
    | ELam [(String, Int)] Expr
    | ELitInt Integer
    | ELitStr String
    | EPrim String [Expr]
    | ECon String [Expr]
    | EUnreachable
    deriving (Show)

data Branch = Branch
    { brCon :: String
    , brBinders :: [(String, Int)]
    , brBody :: Expr
    }
    deriving (Show)

------------------------------------------------------------------------
-- Extraction entry point
------------------------------------------------------------------------

-- | Extract Agda Treeless IR from a source file and emit OrganIR JSON.
extractOrganIR :: FilePath -> IO (Either Text Text)
extractOrganIR inputPath = do
    -- Detect agda version for metadata
    agdaVer <- detectAgdaVersion

    -- Run agda --compile --ghc-dont-call-ghc
    (ec, _stdout, stderrOut) <-
        readProcessWithExitCode
            "agda"
            ["--compile", "--ghc-dont-call-ghc", inputPath]
            ""

    case ec of
        ExitFailure code -> do
            hPutStrLn stderr $ "agda exited with code " ++ show code
            hPutStrLn stderr stderrOut
            pure $ Left $ "agda failed: " <> T.pack stderrOut
        ExitSuccess -> do
            -- Find generated .hs files under MAlonzo/Code/
            let codeDir = "MAlonzo/Code"
            hasCodeDir <- doesDirectoryExist codeDir
            if not hasCodeDir
                then pure $ Left "MAlonzo/Code/ directory not found after compilation"
                else do
                    hsFiles <- findHsFiles codeDir
                    allDefs <- concat <$> mapM parseHsFile hsFiles

                    let modName = takeBaseName inputPath
                        json = emitOrganIR modName inputPath agdaVer allDefs

                    -- Clean up
                    doesDirectoryExist "MAlonzo" >>= \exists ->
                        if exists then removeDirectoryRecursive "MAlonzo" else pure ()

                    pure $ Right json

-- | Detect the agda compiler version string.
detectAgdaVersion :: IO String
detectAgdaVersion = do
    result <- readProcessWithExitCode "agda" ["--version"] ""
    case result of
        (ExitSuccess, out, _) ->
            -- "Agda version 2.8.0\n" -> "agda-2.8.0"
            let ver = case lines out of
                    (l : _) -> dropWhile (not . isDigit) l
                    [] -> ""
             in pure $ "agda-" ++ takeWhile (\c -> isDigit c || c == '.') ver
        _ -> pure "agda-unknown"

-- | Recursively find .hs files under a directory.
findHsFiles :: FilePath -> IO [FilePath]
findHsFiles dir = do
    entries <- listDirectory dir
    let paths = map (\e -> dir ++ "/" ++ e) entries
    files <- concat <$> mapM classifyEntry paths
    pure files
  where
    classifyEntry path
        | ".hs" `isSuffixOf` path = pure [path]
        | otherwise = do
            isDir <- doesDirectoryExist path
            if isDir then findHsFiles path else pure []

------------------------------------------------------------------------
-- MAlonzo Haskell parser
------------------------------------------------------------------------

-- | Parse a MAlonzo-generated Haskell file to extract definitions.
parseHsFile :: FilePath -> IO [MAlonzoDef]
parseHsFile fp = do
    content <- readFile fp
    let ls = lines content
        defs = extractDefs ls
    pure defs

{- | Extract function definitions from MAlonzo output.

MAlonzo definitions look like:
  d_functionName_123 v0 v1 = <body>

We skip module headers, imports, type signatures, pragmas, etc.
-}
extractDefs :: [String] -> [MAlonzoDef]
extractDefs [] = []
extractDefs (l : ls)
    | Just (name, params) <- parseMAlonzoDefHeader l =
        let (bodyLines, rest) = collectDefBody ls
            body = unlines (dropTrailingBlanks (l : bodyLines))
         in MAlonzoDef name params body : extractDefs rest
    | otherwise = extractDefs ls

{- | Try to parse a MAlonzo definition header.
Matches: d_name_123 v0 v1 = ...
Also matches: name v0 v1 = ... (for top-level non-prefixed defs)
-}
parseMAlonzoDefHeader :: String -> Maybe (String, [String])
parseMAlonzoDefHeader line
    | null stripped = Nothing
    | "{" `isPrefixOf` stripped = Nothing -- pragma
    | "--" `isPrefixOf` stripped = Nothing -- comment
    | "import" `isPrefixOf` stripped = Nothing
    | "module" `isPrefixOf` stripped = Nothing
    | "type" `isPrefixOf` stripped = Nothing
    | "data" `isPrefixOf` stripped = Nothing
    | "class" `isPrefixOf` stripped = Nothing
    | "instance" `isPrefixOf` stripped = Nothing
    | "infixl" `isPrefixOf` stripped = Nothing
    | "infixr" `isPrefixOf` stripped = Nothing
    | "infix" `isPrefixOf` stripped = Nothing
    | case line of (c : _) -> isSpace c; [] -> False = Nothing -- continuation / indented line
    | otherwise = parseNameAndParams (words stripped)
  where
    stripped = dropWhile isSpace line

parseNameAndParams :: [String] -> Maybe (String, [String])
parseNameAndParams [] = Nothing
parseNameAndParams (name : rest)
    | not (isMAlonzoName name || isLowerName name) = Nothing
    | otherwise =
        let (params, afterParams) = span isParam rest
         in case afterParams of
                ("=" : _) -> Just (cleanName name, params)
                _ -> Nothing
  where
    isParam s = all (\c -> isAlphaNum c || c == '_') s && not (null s) && s /= "="
    isLowerName [] = False
    isLowerName (c : _) = c >= 'a' && c <= 'z' || c == '_'

-- | Check if a name looks like a MAlonzo-generated name (d_ prefix).
isMAlonzoName :: String -> Bool
isMAlonzoName ('d' : '_' : _) = True
isMAlonzoName ('c' : '_' : _) = True -- constructor wrappers
isMAlonzoName _ = False

-- | Clean a MAlonzo name: d_factorial_123 -> factorial
cleanName :: String -> String
cleanName name
    | "d_" `isPrefixOf` name = stripTrailingId (drop 2 name)
    | "c_" `isPrefixOf` name = stripTrailingId (drop 2 name)
    | otherwise = name
  where
    -- Strip trailing _NNN numeric suffix
    stripTrailingId s =
        let (base, suffix) = breakOnLastUnderscore s
         in if not (null suffix) && all isDigit suffix
                then base
                else s
    breakOnLastUnderscore s =
        let parts = splitOnUnderscore s
         in case unsnoc parts of
                Just (front, end) | not (null front) -> (concatWithUnderscore front, end)
                _ -> (s, "")
    splitOnUnderscore = go [] ""
      where
        go acc cur [] = reverse (reverse cur : acc)
        go acc cur ('_' : cs) = go (reverse cur : acc) "" cs
        go acc cur (c : cs) = go acc (c : cur) cs
    concatWithUnderscore [] = ""
    concatWithUnderscore [x] = x
    concatWithUnderscore (x : xs) = x ++ "_" ++ concatWithUnderscore xs

-- | Collect indented body lines following a definition.
collectDefBody :: [String] -> ([String], [String])
collectDefBody [] = ([], [])
collectDefBody (l : ls)
    | null l -- blank line might separate defs
        =
        let (more, rest) = collectDefBody ls
         in if null more then ([l], ls) else (l : more, rest)
    | case l of (c : _) -> isSpace c; [] -> False -- indented continuation
        =
        let (more, rest) = collectDefBody ls
         in (l : more, rest)
    | otherwise = ([], l : ls)

dropTrailingBlanks :: [String] -> [String]
dropTrailingBlanks = reverse . dropWhile null . reverse

------------------------------------------------------------------------
-- Body expression parser
------------------------------------------------------------------------

{- | Parse a MAlonzo function body into an Expr.
This is a best-effort parser for the generated Haskell patterns.
-}
parseExprInner :: String -> Expr
parseExprInner s
    -- erased
    | s == "erased" = EUnreachable
    -- integer literal
    | all isDigit s, not (null s) = ELitInt (read s)
    -- negative integer
    | "-" `isPrefixOf` s, all isDigit (drop 1 s), length s > 1 = ELitInt (read s)
    -- string literal
    | "\"" `isPrefixOf` s = ELitStr (read s)
    -- let expression
    | "let" `isPrefixOf` s = parseLet s
    -- case expression (strip coe wrapper if present)
    | "case" `isPrefixOf` s = parseCase s
    | "coe" `isPrefixOf` s = parseExprInner (stripCoe s)
    -- primitive operations
    | Just (op, args) <- parsePrimOp s = EPrim op (map parseExprInner args)
    -- variable
    | isSimpleVar s = EVar s 0
    -- application
    | otherwise = parseApp s

-- | Strip surrounding parens.
stripOuter :: String -> String
stripOuter ('(' : s) = case unsnoc s of
    Just (inner, ')') | parenBalanced inner -> stripOuter (trim inner)
    _ -> '(' : s
stripOuter s = s

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x : xs) = case unsnoc xs of
    Just (front, end) -> Just (x : front, end)
    Nothing -> Nothing

parenBalanced :: String -> Bool
parenBalanced = go (0 :: Int)
  where
    go n [] = n >= 0
    go n _ | n < 0 = False
    go n ('(' : cs) = go (n + 1) cs
    go n (')' : cs) = go (n - 1) cs
    go n (_ : cs) = go n cs

-- | Strip `coe` wrapper: "coe v0" -> "v0"
stripCoe :: String -> String
stripCoe s
    | "coe " `isPrefixOf` s = trim (drop 4 s)
    | "coe\n" `isPrefixOf` s = trim (drop 4 s)
    | s == "coe" = ""
    | otherwise = s

-- | Parse a let expression.
parseLet :: String -> Expr
parseLet s =
    -- "let v0 = <bind> in <body>" (simplified)
    case stripPrefix "let " s of
        Nothing -> EVar "let_unparsed" 0
        Just rest ->
            let (binder, afterBinder) = span (\c -> isAlphaNum c || c == '_') (trim rest)
                afterEq = trim $ drop 1 $ dropWhile (/= '=') afterBinder
                -- Find "in" keyword
                (bindStr, bodyStr) = splitOnIn afterEq
             in ELet binder 0 (parseExprInner (trim bindStr)) (parseExprInner (trim bodyStr))

-- | Split on the first top-level "in" keyword.
splitOnIn :: String -> (String, String)
splitOnIn = go (0 :: Int) ""
  where
    go _ acc [] = (reverse acc, "")
    go depth acc (' ' : 'i' : 'n' : ' ' : rest)
        | depth == 0 = (reverse acc, rest)
    go depth acc ('(' : rest) = go (depth + 1) ('(' : acc) rest
    go depth acc (')' : rest) = go (depth - 1) (')' : acc) rest
    go depth acc (c : rest) = go depth (c : acc) rest

-- | Parse a case expression.
parseCase :: String -> Expr
parseCase s =
    -- "case <scrutinee> of { <alts> }"
    let afterCase = trim $ fromMaybe s (stripPrefix "case " s)
        -- Strip possible "coe" in scrutinee: "case coe v0 of ..."
        afterCase' =
            if "coe " `isPrefixOf` afterCase
                then trim (drop 4 afterCase)
                else afterCase
        (scrutinee, afterOf) = break (== ' ') afterCase'
        bodyPart = trim $ fromMaybe afterOf (stripPrefix " of" (trim afterOf))
     in ECase (trim scrutinee) 0 (parseBranches bodyPart)

{- | Parse case branches from MAlonzo output.
MAlonzo generates patterns like:
  C_con_123 v1 v2 -> <body>
  _ -> <default>
-}
parseBranches :: String -> [Branch]
parseBranches s =
    let cleaned = stripOuter (trim s)
        -- Split on newlines that start at the same indent level
        branchLines = splitBranches cleaned
     in map parseSingleBranch branchLines

splitBranches :: String -> [String]
splitBranches s =
    let ls = lines s
     in -- Group lines: a branch starts with a non-blank, non-indented line
        -- or after a semicolon separator
        case ls of
            [] -> []
            _ -> groupBranches ls

groupBranches :: [String] -> [String]
groupBranches [] = []
groupBranches (l : ls) =
    let (cont, rest) = span isContinuation ls
     in unlines (l : cont) : groupBranches rest
  where
    isContinuation "" = False
    isContinuation (c : _) = isSpace c

parseSingleBranch :: String -> Branch
parseSingleBranch s =
    let trimmed = trim s
        (patStr, bodyStr) = splitOnArrow trimmed
        (con, binders) = parsePattern (trim patStr)
     in Branch con binders (parseExprInner (trim bodyStr))

-- | Split on " -> " at the top level.
splitOnArrow :: String -> (String, String)
splitOnArrow = go (0 :: Int) ""
  where
    go _ acc [] = (reverse acc, "")
    go depth acc (' ' : '-' : '>' : ' ' : rest)
        | depth == 0 = (reverse acc, rest)
    go depth acc ('(' : rest) = go (depth + 1) ('(' : acc) rest
    go depth acc (')' : rest) = go (depth - 1) (')' : acc) rest
    go depth acc (c : rest) = go depth (c : acc) rest

-- | Parse a pattern: "C_con_123 v0 v1" -> ("con", [("v0",0), ("v1",0)])
parsePattern :: String -> (String, [(String, Int)])
parsePattern s =
    case words s of
        [] -> ("_", [])
        [w] -> (cleanName w, [])
        (con : vs) -> (cleanName con, map (\v -> (v, 0)) vs)

-- | Detect primitive operations.
parsePrimOp :: String -> Maybe (String, [String])
parsePrimOp s = case s of
    _
        | "addInt " `isPrefixOf` s -> Just ("add", twoArgs (drop 7 s))
        | "subInt " `isPrefixOf` s -> Just ("sub", twoArgs (drop 7 s))
        | "mulInt " `isPrefixOf` s -> Just ("mul", twoArgs (drop 7 s))
        | "divInt " `isPrefixOf` s -> Just ("div", twoArgs (drop 7 s))
        | "modInt " `isPrefixOf` s -> Just ("mod", twoArgs (drop 7 s))
        | "eqInt " `isPrefixOf` s -> Just ("eq", twoArgs (drop 6 s))
        | "ltInt " `isPrefixOf` s -> Just ("lt", twoArgs (drop 6 s))
        | "geqInt " `isPrefixOf` s -> Just ("geq", twoArgs (drop 7 s))
        | "negateInt " `isPrefixOf` s -> Just ("neg", [trim (drop 10 s)])
        | "quotInt " `isPrefixOf` s -> Just ("quot", twoArgs (drop 8 s))
        | "remInt " `isPrefixOf` s -> Just ("rem", twoArgs (drop 7 s))
    _ -> Nothing
  where
    twoArgs rest =
        let ws = words rest
         in case ws of
                (a : b : _) -> [a, b]
                [a] -> [a]
                [] -> []

-- | Check if a string is a simple variable name.
isSimpleVar :: String -> Bool
isSimpleVar [] = False
isSimpleVar s = all (\c -> isAlphaNum c || c == '_') s

-- | Parse a function application: "f x y z" -> EApp "f" 0 [args]
parseApp :: String -> Expr
parseApp s =
    let ws = words s
     in case ws of
            [] -> EUnreachable
            [w] -> EVar w 0
            (f : args) -> EApp (cleanName f) 0 (map (\a -> parseExprInner (stripOuter (trim a))) args)

------------------------------------------------------------------------
-- OrganIR JSON emission
------------------------------------------------------------------------

-- | Emit complete OrganIR JSON using organ-ir library.
emitOrganIR :: String -> FilePath -> String -> [MAlonzoDef] -> Text
emitOrganIR modName srcFile agdaVer defs =
    let metadata = IR.Metadata IR.LAgda (Just (T.pack agdaVer)) (Just (T.pack srcFile)) "agda-shim-0.1" Nothing
     in renderOrganIR $ IR.OrganIR metadata (IR.Module (T.pack modName) [] (zipWith defToIR [1 ..] defs) [] [])

defToIR :: Int -> MAlonzoDef -> IR.Definition
defToIR uid def =
    IR.Definition
        { IR.defName = IR.QName "" (IR.Name (T.pack (mdName def)) uid)
        , IR.defType = IR.TAny
        , IR.defExpr = exprToIR (parseBody def)
        , IR.defSort = IR.SFun
        , IR.defVisibility = IR.Public
        , IR.defArity = length (mdParams def)
        }

-- | Parse the body of a definition into an Expr.
parseBody :: MAlonzoDef -> Expr
parseBody def =
    let params = mdParams def
        raw = mdBody def
        -- Extract body after the "=" sign on the first line
        bodyStr = case break (== '=') raw of
            (_, '=' : rest) -> trim rest
            _ -> raw
     in if null params
            then parseExprInner (stripOuter (trim bodyStr))
            else ELam (map (\p -> (p, 0)) params) (parseExprInner (stripOuter (trim bodyStr)))

-- | Translate local Expr to OrganIR Expr.
exprToIR :: Expr -> IR.Expr
exprToIR (EVar name uid) = IR.EVar (IR.Name (T.pack name) uid)
exprToIR (EApp name uid args) = IR.EApp (IR.EVar (IR.Name (T.pack name) uid)) (map exprToIR args)
exprToIR (ELet name uid bind body) = IR.ELet [IR.LetBind (IR.Name (T.pack name) uid) Nothing (exprToIR bind)] (exprToIR body)
exprToIR (ECase scrut uid branches) = IR.ECase (IR.EVar (IR.Name (T.pack scrut) uid)) (map branchToIR branches)
exprToIR (ELam params body) = IR.ELam (map (\(n, u) -> IR.LamParam (IR.Name (T.pack n) u) Nothing) params) (exprToIR body)
exprToIR (ELitInt n) = IR.ELit (IR.LitInt n)
exprToIR (ELitStr s) = IR.ELit (IR.LitString (T.pack s))
exprToIR (EPrim op args) = IR.EApp (IR.EVar (IR.Name (T.pack op) 0)) (map exprToIR args)
exprToIR (ECon name args) = IR.ECon (IR.localName (T.pack name)) (map exprToIR args)
exprToIR EUnreachable = IR.EUnreachable

-- | Translate local Branch to OrganIR Branch.
branchToIR :: Branch -> IR.Branch
branchToIR br =
    IR.Branch
        (IR.PatCon (IR.localName (T.pack (brCon br))) (map (\(n, u) -> IR.PatBinder (IR.Name (T.pack n) u) Nothing) (brBinders br)))
        (exprToIR (brBody br))

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

stripPrefix :: String -> String -> Maybe String
stripPrefix [] ys = Just ys
stripPrefix (_ : _) [] = Nothing
stripPrefix (x : xs) (y : ys)
    | x == y = stripPrefix xs ys
    | otherwise = Nothing

fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing = d
fromMaybe _ (Just x) = x
