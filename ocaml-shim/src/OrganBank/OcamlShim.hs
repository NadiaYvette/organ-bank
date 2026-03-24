{- | OCaml Lambda IR Extraction Shim

Invokes @ocamlopt -dlambda -c@ on an OCaml source file to dump the
Lambda intermediate representation, then parses the S-expression-like
output and emits OrganIR JSON.

OCaml Lambda IR is post-type-erasure: all types are erased.  The IR
uses @let@/@letrec@ bindings with @(function ...)@ forms for lambdas
and @(makeblock 0 ...)@ for the module export tuple.
-}
module OrganBank.OcamlShim (
    extractOrganIR,
) where

import Data.Char (isAlphaNum, isAsciiLower, isDigit, isSpace)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName)
import System.Process (readProcessWithExitCode)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Extract OCaml Lambda IR from a @.ml@ file and return OrganIR JSON.
extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
    -- ocamlopt -dlambda prints Lambda IR to stdout, compiles to .cmx/.o
    -- We use -c to avoid linking.
    (exitCode, stdout_, stderr_) <-
        readProcessWithExitCode "ocamlopt" ["-dlambda", "-c", inputPath] ""
    -- Lambda IR goes to stderr for some OCaml versions, stdout for others.
    -- Try stderr first; if empty, use stdout.
    let lambdaText = if null (strip stderr_) then stdout_ else stderr_
    case exitCode of
        ExitSuccess -> do
            let modName = capitalise (takeBaseName inputPath)
                defs = parseLambdaIR lambdaText
                exports = extractExports lambdaText
                json = emitOrganIR modName inputPath defs exports
            pure (Right json)
        ExitFailure _ ->
            -- Even when compilation fails, -dlambda may still have printed IR
            -- before the error.  But if there's nothing useful, report the error.
            if null (strip lambdaText)
                then pure (Left $ "ocamlopt failed:\n" <> stderr_ <> stdout_)
                else do
                    let modName = capitalise (takeBaseName inputPath)
                        defs = parseLambdaIR lambdaText
                        exports = extractExports lambdaText
                        json = emitOrganIR modName inputPath defs exports
                    pure (Right json)

-- ---------------------------------------------------------------------------
-- Lambda IR types
-- ---------------------------------------------------------------------------

-- | A parsed top-level definition from the Lambda IR.
data LambdaDef = LambdaDef
    { ldName :: String
    -- ^ Bare name (e.g. "fact")
    , ldUnique :: Int
    -- ^ OCaml stamp (e.g. 270 from "fact/270")
    , ldArity :: Int
    -- ^ Number of parameters (0 for non-functions)
    , ldParams :: [OcamlParam]
    -- ^ Parameter names/uniques
    , ldIsRec :: Bool
    -- ^ Bound in a letrec?
    , ldBody :: LambdaExpr
    -- ^ Parsed body expression
    }
    deriving (Show)

data OcamlParam = OcamlParam
    { opName :: String
    , opUnique :: Int
    }
    deriving (Show)

-- | Simplified Lambda expression tree.
data LambdaExpr
    = -- | Variable reference: name/stamp
      LVar String Int
    | -- | Integer literal
      LInt Int
    | -- | Float literal
      LFloat Double
    | -- | String literal
      LString String
    | -- | Application
      LApp LambdaExpr [LambdaExpr]
    | -- | Function (lambda)
      LLam [OcamlParam] LambdaExpr
    | -- | Let binding
      LLet [(String, Int, LambdaExpr)] LambdaExpr
    | -- | Letrec
      LLetRec [(String, Int, LambdaExpr)] LambdaExpr
    | -- | If-then-else
      LIf LambdaExpr LambdaExpr LambdaExpr
    | -- | Switch/match
      LSwitch LambdaExpr [(Int, LambdaExpr)]
    | -- | (makeblock tag ...)
      LMakeBlock Int [LambdaExpr]
    | -- | Primitive operation
      LPrim String [LambdaExpr]
    | -- | Sequence
      LSeq LambdaExpr LambdaExpr
    | -- | Unparsed fallback
      LRaw String
    deriving (Show)

-- ---------------------------------------------------------------------------
-- S-expression tokenizer
-- ---------------------------------------------------------------------------

data SExpr
    = SAtom String
    | SList [SExpr]
    deriving (Show)

tokenize :: String -> [String]
tokenize [] = []
tokenize ('(' : rest) = "(" : tokenize rest
tokenize (')' : rest) = ")" : tokenize rest
tokenize ('"' : rest) =
    let (s, rest') = spanString rest
     in ('"' : s ++ "\"") : tokenize rest'
tokenize (c : rest)
    | isSpace c = tokenize rest
    | otherwise =
        let (tok, rest') = span isAtomChar (c : rest)
         in tok : tokenize rest'
  where
    isAtomChar x = not (isSpace x) && x /= '(' && x /= ')' && x /= '"'

-- | Consume a string literal body (after opening quote).
spanString :: String -> (String, String)
spanString [] = ([], [])
spanString ('"' : rest) = ([], rest)
spanString ('\\' : c : rest) = let (s, r) = spanString rest in ('\\' : c : s, r)
spanString (c : rest) = let (s, r) = spanString rest in (c : s, r)

parseSExprs :: [String] -> ([SExpr], [String])
parseSExprs [] = ([], [])
parseSExprs (")" : rest) = ([], rest)
parseSExprs ("(" : rest) =
    let (children, rest') = parseSExprs rest
        (siblings, rest'') = parseSExprs rest'
     in (SList children : siblings, rest'')
parseSExprs (tok : rest) =
    let (siblings, rest') = parseSExprs rest
     in (SAtom tok : siblings, rest')

parseSExprTop :: String -> [SExpr]
parseSExprTop input = fst (parseSExprs (tokenize input))

-- ---------------------------------------------------------------------------
-- Lambda IR parsing
-- ---------------------------------------------------------------------------

-- | Parse top-level definitions from the Lambda IR text.
parseLambdaIR :: String -> [LambdaDef]
parseLambdaIR input =
    let sexprs = parseSExprTop input
     in concatMap extractDefsFromSExpr sexprs

extractDefsFromSExpr :: SExpr -> [LambdaDef]
extractDefsFromSExpr (SList (SAtom "let" : rest)) =
    extractLetDefs False rest
extractDefsFromSExpr (SList (SAtom "letrec" : rest)) =
    extractLetDefs True rest
extractDefsFromSExpr _ = []

{- | Extract definitions from a let/letrec form.
The form is: (let (binding1 binding2 ...) body)
or nested: (let (name/N = expr) body)
OCaml Lambda uses: (let (name/N expr) rest) where rest is the continuation
-}
extractLetDefs :: Bool -> [SExpr] -> [LambdaDef]
extractLetDefs isRec rest =
    case rest of
        -- (let/letrec (name/N stuff...) body)
        (SList binding : body) ->
            let defs = extractBinding isRec binding
                -- The body itself may contain more let/letrec forms
                defs' = concatMap extractDefsFromSExpr body
             in defs ++ defs'
        _ -> []

extractBinding :: Bool -> [SExpr] -> [LambdaDef]
extractBinding isRec (SAtom nameSlash : SAtom "=" : exprParts) =
    let (name, uniq) = parseNameSlash nameSlash
        expr = parseLambdaExpr (regroup exprParts)
        (arity, params) = extractFunctionInfo expr
     in [LambdaDef name uniq arity params isRec expr]
extractBinding isRec (SAtom nameSlash : exprParts) =
    let (name, uniq) = parseNameSlash nameSlash
        expr = parseLambdaExpr (regroup exprParts)
        (arity, params) = extractFunctionInfo expr
     in [LambdaDef name uniq arity params isRec expr]
extractBinding _ _ = []

-- | Regroup a list of SExprs into a single one for parsing.
regroup :: [SExpr] -> SExpr
regroup [x] = x
regroup xs = SList xs

-- | Parse a Lambda expression from an S-expression.
parseLambdaExpr :: SExpr -> LambdaExpr
parseLambdaExpr (SAtom s)
    | all isDigit s = LInt (read s)
    | isNegInt s = LInt (read s)
    | isFloat s = LFloat (read s)
    | isStringLit s = LString (unquote s)
    | '/' `elem` s = let (n, u) = parseNameSlash s in LVar n u
    | otherwise = LVar s 0
  where
    isNegInt ('-' : cs) = not (null cs) && all isDigit cs
    isNegInt _ = False
    isFloat xs = case break (== '.') xs of
        (pre, '.' : post) ->
            not (null pre)
                && not (null post)
                && all (\c -> isDigit c || c == '-') pre
                && all isDigit post
        _ -> False
    isStringLit ('"' : _) = True
    isStringLit _ = False
    unquote ('"' : xs') = take (length xs' - 1) xs'
    unquote xs' = xs'
parseLambdaExpr (SList (SAtom "function" : rest)) =
    let (params, body) = spanParams rest
     in LLam params (parseLambdaExpr (regroup body))
  where
    spanParams :: [SExpr] -> ([OcamlParam], [SExpr])
    spanParams (SAtom p : SAtom ":" : SAtom _kind : more) =
        let (ps, b) = spanParams more
            (n, u) = parseNameSlash p
         in (OcamlParam n u : ps, b)
    spanParams (SAtom p : more)
        | '/' `elem` p =
            -- Could be a param without kind annotation
            let (ps, b) = spanParams more
                (n, u) = parseNameSlash p
             in (OcamlParam n u : ps, b)
    spanParams xs = ([], xs)
parseLambdaExpr (SList (SAtom "let" : rest)) =
    case rest of
        (SList binding : body) ->
            let binds = extractLetBindings binding
                bodyExpr = parseLambdaExpr (regroup body)
             in LLet binds bodyExpr
        _ -> LRaw (show rest)
parseLambdaExpr (SList (SAtom "letrec" : rest)) =
    case rest of
        (SList binding : body) ->
            let binds = extractLetBindings binding
                bodyExpr = parseLambdaExpr (regroup body)
             in LLetRec binds bodyExpr
        _ -> LRaw (show rest)
parseLambdaExpr (SList [SAtom "if", cond, SAtom "then", thn, SAtom "else", els]) =
    LIf (parseLambdaExpr cond) (parseLambdaExpr thn) (parseLambdaExpr els)
parseLambdaExpr (SList (SAtom "if" : cond : rest)) =
    -- Flexible if parsing
    case rest of
        (thn : els : _) -> LIf (parseLambdaExpr cond) (parseLambdaExpr thn) (parseLambdaExpr els)
        _ -> LRaw ("if " ++ show rest)
parseLambdaExpr (SList (SAtom "makeblock" : SAtom tag : fields))
    | all isDigit tag =
        LMakeBlock (read tag) (map parseLambdaExpr fields)
parseLambdaExpr (SList (SAtom "seq" : rest)) =
    case rest of
        [a, b] -> LSeq (parseLambdaExpr a) (parseLambdaExpr b)
        _ -> LRaw ("seq " ++ show rest)
parseLambdaExpr (SList (SAtom "apply" : fn : args)) =
    LApp (parseLambdaExpr fn) (map parseLambdaExpr args)
parseLambdaExpr (SList (SAtom prim : args))
    | isPrimName prim =
        LPrim prim (map parseLambdaExpr args)
parseLambdaExpr (SList (fn : args)) =
    LApp (parseLambdaExpr fn) (map parseLambdaExpr args)
parseLambdaExpr (SList []) = LRaw "()"

-- | Is this an OCaml primitive name? (starts with % or caml_)
isPrimName :: String -> Bool
isPrimName ('%' : _) = True
isPrimName s = "caml_" `isPrefixOf` s

extractLetBindings :: [SExpr] -> [(String, Int, LambdaExpr)]
extractLetBindings (SAtom nameSlash : SAtom "=" : rest) =
    let (exprParts, remaining) = splitBinding rest
        (name, uniq) = parseNameSlash nameSlash
        expr = parseLambdaExpr (regroup exprParts)
     in (name, uniq, expr) : extractLetBindings remaining
extractLetBindings (SAtom nameSlash : rest) =
    let (exprParts, remaining) = splitBinding rest
        (name, uniq) = parseNameSlash nameSlash
        expr = parseLambdaExpr (regroup exprParts)
     in (name, uniq, expr) : extractLetBindings remaining
extractLetBindings _ = []

-- | Split binding list at the next binding (a bare name/N atom).
splitBinding :: [SExpr] -> ([SExpr], [SExpr])
splitBinding [] = ([], [])
splitBinding (s@(SAtom a) : rest)
    | '/' `elem` a
    , isBindName a =
        -- This looks like the next binding name
        ([], s : rest)
  where
    isBindName xs =
        let (n, _) = parseNameSlash xs
         in not (null n) && all (\c -> isAlphaNum c || c == '_' || c == '\'') n
splitBinding (x : rest) =
    let (expr, remaining) = splitBinding rest
     in (x : expr, remaining)

-- | Extract function arity and parameters from a lambda expression.
extractFunctionInfo :: LambdaExpr -> (Int, [OcamlParam])
extractFunctionInfo (LLam params _) = (length params, params)
extractFunctionInfo _ = (0, [])

{- | Parse "name/123" or "name/123[type]" into (name, 123).
Falls back to (s, 0) if no slash.
-}
parseNameSlash :: String -> (String, Int)
parseNameSlash s =
    case break (== '/') s of
        (name, '/' : rest) ->
            -- Strip optional [type] annotation: "123[int]" -> "123"
            let numStr = takeWhile isDigit rest
             in if not (null numStr)
                    then (name, read numStr)
                    else (s, 0)
        _ -> (s, 0)

-- | Extract the module exports list from a @(makeblock 0 ...)@ at the end.
extractExports :: String -> [String]
extractExports input =
    let sexprs = parseSExprTop input
     in extractExportsFromSExprs sexprs

extractExportsFromSExprs :: [SExpr] -> [String]
extractExportsFromSExprs [] = []
extractExportsFromSExprs sexprs =
    -- Walk all top-level forms and nested let/letrec bodies to find
    -- the final makeblock
    concatMap findMakeBlock sexprs

findMakeBlock :: SExpr -> [String]
findMakeBlock (SList (SAtom "makeblock" : SAtom "0" : fields)) =
    mapMaybe extractExportName fields
  where
    mapMaybe _ [] = []
    mapMaybe f (x : xs) = case f x of
        Just v -> v : mapMaybe f xs
        Nothing -> mapMaybe f xs
    extractExportName (SAtom nameSlash) = Just (fst (parseNameSlash nameSlash))
    extractExportName _ = Nothing
findMakeBlock (SList (SAtom "let" : _ : body)) =
    concatMap findMakeBlock body
findMakeBlock (SList (SAtom "letrec" : _ : body)) =
    concatMap findMakeBlock body
findMakeBlock (SList (SAtom "seq" : rest)) =
    concatMap findMakeBlock rest
findMakeBlock _ = []

-- ---------------------------------------------------------------------------
-- OrganIR emission via organ-ir library
-- ---------------------------------------------------------------------------

emitOrganIR :: String -> FilePath -> [LambdaDef] -> [String] -> Text
emitOrganIR modName srcFile defs exports =
    let meta =
            IR.Metadata
                IR.LOCaml
                Nothing
                (Just (T.pack srcFile))
                "ocaml-shim-0.1"
                Nothing
        modul =
            IR.Module
                (T.pack modName)
                (map T.pack exports)
                (map (defToIR modName) defs)
                []
                []
     in renderOrganIR (IR.OrganIR meta modul)

defToIR :: String -> LambdaDef -> IR.Definition
defToIR modName def =
    let qname = IR.QName (T.pack modName) (IR.Name (T.pack (ldName def)) (ldUnique def))
        arity = ldArity def
        ty
            | arity > 0 = IR.TFn (map paramToArg (ldParams def)) IR.pureEffect IR.TAny
            | otherwise = IR.TAny
        sort_
            | arity > 0 = IR.SFun
            | otherwise = IR.SVal
     in IR.Definition qname ty (exprToIR (ldBody def)) sort_ IR.Public arity

paramToArg :: OcamlParam -> IR.FnArg
paramToArg _ = IR.FnArg (Just IR.Many) IR.TAny

exprToIR :: LambdaExpr -> IR.Expr
exprToIR (LVar n u) = IR.EVar (IR.Name (T.pack n) u)
exprToIR (LInt n) = IR.ELit (IR.LitInt (fromIntegral n))
exprToIR (LFloat f) = IR.ELit (IR.LitFloat f)
exprToIR (LString s) = IR.ELit (IR.LitString (T.pack s))
exprToIR (LLam params body) = IR.ELam (map paramToLamParam params) (exprToIR body)
exprToIR (LApp fn args) = IR.EApp (exprToIR fn) (map exprToIR args)
exprToIR (LLet binds body) = IR.ELet (map bindToIR binds) (exprToIR body)
exprToIR (LLetRec binds body) = IR.ELet (map bindToIR binds) (exprToIR body)
exprToIR (LIf cond thn els) =
    IR.ECase
        (exprToIR cond)
        [ IR.Branch (IR.PatLit (IR.LitBool True)) (exprToIR thn)
        , IR.Branch (IR.PatLit (IR.LitBool False)) (exprToIR els)
        ]
exprToIR (LSwitch scrut branches) =
    IR.ECase
        (exprToIR scrut)
        ( map
            (\(tag, body) -> IR.Branch (IR.PatLit (IR.LitInt (fromIntegral tag))) (exprToIR body))
            branches
        )
exprToIR (LMakeBlock tag fields) =
    IR.ECon (IR.qualName "ocaml" ("block_" <> T.pack (show tag))) (map exprToIR fields)
exprToIR (LPrim n args) = IR.EApp (IR.EVar (IR.Name (T.pack n) 0)) (map exprToIR args)
exprToIR (LSeq a b) = IR.ELet [IR.LetBind (IR.Name "_seq" 0) Nothing (exprToIR a)] (exprToIR b)
exprToIR (LRaw s) = IR.ELit (IR.LitString (T.pack s))

paramToLamParam :: OcamlParam -> IR.LamParam
paramToLamParam p = IR.LamParam (IR.Name (T.pack (opName p)) (opUnique p)) Nothing

bindToIR :: (String, Int, LambdaExpr) -> IR.LetBind
bindToIR (n, u, e) = IR.LetBind (IR.Name (T.pack n) u) Nothing (exprToIR e)

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
