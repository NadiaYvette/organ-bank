-- | OCaml Lambda IR Extraction Shim
--
-- Invokes @ocamlopt -dlambda -c@ on an OCaml source file to dump the
-- Lambda intermediate representation, then parses the S-expression-like
-- output and emits OrganIR JSON.
--
-- OCaml Lambda IR is post-type-erasure: all types are erased.  The IR
-- uses @let@/@letrec@ bindings with @(function ...)@ forms for lambdas
-- and @(makeblock 0 ...)@ for the module export tuple.

module OrganBank.OcamlShim
  ( extractOrganIR
  ) where

import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (isPrefixOf, intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeBaseName)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

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
      let modName   = capitalise (takeBaseName inputPath)
          defs      = parseLambdaIR lambdaText
          exports   = extractExports lambdaText
          json      = emitOrganIR modName inputPath defs exports
      pure (Right json)
    ExitFailure _ ->
      -- Even when compilation fails, -dlambda may still have printed IR
      -- before the error.  But if there's nothing useful, report the error.
      if null (strip lambdaText)
        then pure (Left $ "ocamlopt failed:\n" <> stderr_ <> stdout_)
        else do
          let modName = capitalise (takeBaseName inputPath)
              defs    = parseLambdaIR lambdaText
              exports = extractExports lambdaText
              json    = emitOrganIR modName inputPath defs exports
          pure (Right json)

-- ---------------------------------------------------------------------------
-- Lambda IR types
-- ---------------------------------------------------------------------------

-- | A parsed top-level definition from the Lambda IR.
data LambdaDef = LambdaDef
  { ldName   :: String   -- ^ Bare name (e.g. "fact")
  , ldUnique :: Int      -- ^ OCaml stamp (e.g. 270 from "fact/270")
  , ldArity  :: Int      -- ^ Number of parameters (0 for non-functions)
  , ldParams :: [OcamlParam]  -- ^ Parameter names/uniques
  , ldIsRec  :: Bool     -- ^ Bound in a letrec?
  , ldBody   :: LambdaExpr    -- ^ Parsed body expression
  } deriving (Show)

data OcamlParam = OcamlParam
  { opName   :: String
  , opUnique :: Int
  } deriving (Show)

-- | Simplified Lambda expression tree.
data LambdaExpr
  = LVar String Int            -- ^ Variable reference: name/stamp
  | LInt Int                   -- ^ Integer literal
  | LFloat Double              -- ^ Float literal
  | LString String             -- ^ String literal
  | LApp LambdaExpr [LambdaExpr]  -- ^ Application
  | LLam [OcamlParam] LambdaExpr  -- ^ Function (lambda)
  | LLet [(String, Int, LambdaExpr)] LambdaExpr  -- ^ Let binding
  | LLetRec [(String, Int, LambdaExpr)] LambdaExpr  -- ^ Letrec
  | LIf LambdaExpr LambdaExpr LambdaExpr  -- ^ If-then-else
  | LSwitch LambdaExpr [(Int, LambdaExpr)]  -- ^ Switch/match
  | LMakeBlock Int [LambdaExpr]  -- ^ (makeblock tag ...)
  | LPrim String [LambdaExpr]   -- ^ Primitive operation
  | LSeq LambdaExpr LambdaExpr  -- ^ Sequence
  | LRaw String                -- ^ Unparsed fallback
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
tokenize ('(':rest) = "(" : tokenize rest
tokenize (')':rest) = ")" : tokenize rest
tokenize ('"':rest) = let (s, rest') = spanString rest
                      in ('"' : s ++ "\"") : tokenize rest'
tokenize (c:rest)
  | isSpace c = tokenize rest
  | otherwise = let (tok, rest') = span isAtomChar (c:rest)
                in tok : tokenize rest'
  where
    isAtomChar x = not (isSpace x) && x /= '(' && x /= ')' && x /= '"'

-- | Consume a string literal body (after opening quote).
spanString :: String -> (String, String)
spanString [] = ([], [])
spanString ('"':rest) = ([], rest)
spanString ('\\':c:rest) = let (s, r) = spanString rest in ('\\':c:s, r)
spanString (c:rest) = let (s, r) = spanString rest in (c:s, r)

parseSExprs :: [String] -> ([SExpr], [String])
parseSExprs [] = ([], [])
parseSExprs (")":rest) = ([], rest)
parseSExprs ("(":rest) =
  let (children, rest') = parseSExprs rest
      (siblings, rest'') = parseSExprs rest'
  in (SList children : siblings, rest'')
parseSExprs (tok:rest) =
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

-- | Extract definitions from a let/letrec form.
-- The form is: (let (binding1 binding2 ...) body)
-- or nested: (let (name/N = expr) body)
-- OCaml Lambda uses: (let (name/N expr) rest) where rest is the continuation
extractLetDefs :: Bool -> [SExpr] -> [LambdaDef]
extractLetDefs isRec rest =
  case rest of
    -- (let/letrec (name/N stuff...) body)
    (SList binding : body) ->
      let defs  = extractBinding isRec binding
          -- The body itself may contain more let/letrec forms
          defs' = concatMap extractDefsFromSExpr body
      in defs ++ defs'
    _ -> []

extractBinding :: Bool -> [SExpr] -> [LambdaDef]
extractBinding isRec (SAtom nameSlash : SAtom "=" : exprParts) =
  let (name, uniq) = parseNameSlash nameSlash
      expr         = parseLambdaExpr (regroup exprParts)
      (arity, params) = extractFunctionInfo expr
  in [LambdaDef name uniq arity params isRec expr]
extractBinding isRec (SAtom nameSlash : exprParts) =
  let (name, uniq) = parseNameSlash nameSlash
      expr         = parseLambdaExpr (regroup exprParts)
      (arity, params) = extractFunctionInfo expr
  in [LambdaDef name uniq arity params isRec expr]
extractBinding _ _ = []

-- | Regroup a list of SExprs into a single one for parsing.
regroup :: [SExpr] -> SExpr
regroup [x] = x
regroup xs  = SList xs

-- | Parse a Lambda expression from an S-expression.
parseLambdaExpr :: SExpr -> LambdaExpr
parseLambdaExpr (SAtom s)
  | all isDigit s = LInt (read s)
  | isNegInt s    = LInt (read s)
  | isFloat s     = LFloat (read s)
  | isStringLit s = LString (unquote s)
  | '/' `elem` s  = let (n, u) = parseNameSlash s in LVar n u
  | otherwise     = LVar s 0
  where
    isNegInt ('-':cs) = not (null cs) && all isDigit cs
    isNegInt _        = False
    isFloat xs = case break (== '.') xs of
      (pre, '.':post) -> not (null pre) && not (null post)
                         && all (\c -> isDigit c || c == '-') pre
                         && all isDigit post
      _ -> False
    isStringLit ('"':_) = True
    isStringLit _       = False
    unquote ('"':xs') = take (length xs' - 1) xs'
    unquote xs'       = xs'

parseLambdaExpr (SList (SAtom "function" : rest)) =
  let (params, body) = spanParams rest
  in LLam params (parseLambdaExpr (regroup body))
  where
    spanParams :: [SExpr] -> ([OcamlParam], [SExpr])
    spanParams (SAtom p : SAtom ":" : SAtom _kind : more) =
      let (ps, b) = spanParams more
          (n, u)  = parseNameSlash p
      in (OcamlParam n u : ps, b)
    spanParams (SAtom p : more)
      | '/' `elem` p =
          -- Could be a param without kind annotation
          let (ps, b) = spanParams more
              (n, u)  = parseNameSlash p
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
    _               -> LRaw ("if " ++ show rest)

parseLambdaExpr (SList (SAtom "makeblock" : SAtom tag : fields))
  | all isDigit tag =
      LMakeBlock (read tag) (map parseLambdaExpr fields)

parseLambdaExpr (SList (SAtom "seq" : rest)) =
  case rest of
    [a, b] -> LSeq (parseLambdaExpr a) (parseLambdaExpr b)
    _      -> LRaw ("seq " ++ show rest)

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
isPrimName ('%':_) = True
isPrimName s       = "caml_" `isPrefixOf` s

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
  | '/' `elem` a, isBindName a =
      -- This looks like the next binding name
      ([], s : rest)
  where
    isBindName xs = let (n, _) = parseNameSlash xs
                    in not (null n) && all (\c -> isAlphaNum c || c == '_' || c == '\'') n
splitBinding (x:rest) =
  let (expr, remaining) = splitBinding rest
  in (x : expr, remaining)

-- | Extract function arity and parameters from a lambda expression.
extractFunctionInfo :: LambdaExpr -> (Int, [OcamlParam])
extractFunctionInfo (LLam params _) = (length params, params)
extractFunctionInfo _               = (0, [])

-- | Parse "name/123" or "name/123[type]" into (name, 123).
-- Falls back to (s, 0) if no slash.
parseNameSlash :: String -> (String, Int)
parseNameSlash s =
  case break (== '/') s of
    (name, '/':rest) ->
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
    mapMaybe f (x:xs) = case f x of
      Just v  -> v : mapMaybe f xs
      Nothing -> mapMaybe f xs
    extractExportName (SAtom nameSlash) = Just (fst (parseNameSlash nameSlash))
    extractExportName _                 = Nothing
findMakeBlock (SList (SAtom "let" : _ : body)) =
  concatMap findMakeBlock body
findMakeBlock (SList (SAtom "letrec" : _ : body)) =
  concatMap findMakeBlock body
findMakeBlock (SList (SAtom "seq" : rest)) =
  concatMap findMakeBlock rest
findMakeBlock _ = []

-- ---------------------------------------------------------------------------
-- OrganIR JSON emission
-- ---------------------------------------------------------------------------

emitOrganIR :: String -> FilePath -> [LambdaDef] -> [String] -> Text
emitOrganIR modName srcFile defs exports =
  T.pack $ unlines
    [ "{"
    , "  \"schema_version\": \"1.0.0\","
    , "  \"metadata\": {"
    , "    \"source_language\": \"ocaml\","
    , "    \"source_file\": " ++ jsonString srcFile ++ ","
    , "    \"shim_version\": \"0.1.0\""
    , "  },"
    , "  \"module\": {"
    , "    \"name\": " ++ jsonString modName ++ ","
    , "    \"exports\": [" ++ intercalate ", " (map jsonString exports) ++ "],"
    , "    \"definitions\": ["
    , intercalate ",\n" (map (emitDef modName) defs)
    , "    ],"
    , "    \"data_types\": [],"
    , "    \"effect_decls\": []"
    , "  }"
    , "}"
    ]

emitDef :: String -> LambdaDef -> String
emitDef modName def =
  let name   = ldName def
      uniq   = ldUnique def
      arity  = ldArity def
      params = ldParams def
      sort_  = if arity > 0 then "fun" else "val"
      vis    = "public"
      ty     = if arity > 0
               then emitFnType params
               else "      {\"con\": {\"qname\": {\"module\": \"std\", \"name\": {\"text\": \"any\"}}}}"
      expr   = emitExpr (ldBody def)
  in unlines
    [ "      {"
    , "        \"name\": {\"module\": " ++ jsonString modName
        ++ ", \"name\": {\"text\": " ++ jsonString name
        ++ ", \"unique\": " ++ show uniq ++ "}},"
    , "        \"type\": " ++ ty ++ ","
    , "        \"expr\": " ++ expr ++ ","
    , "        \"sort\": " ++ jsonString sort_ ++ ","
    , "        \"visibility\": " ++ jsonString vis
    , "      }"
    ]

-- | Emit a function type with the given params. All types are @any@ since
-- OCaml Lambda IR is post-type-erasure.
emitFnType :: [OcamlParam] -> String
emitFnType params =
  "{\"fn\": {\"args\": ["
  ++ intercalate ", " (map emitArgType params)
  ++ "], \"effect\": {\"effects\": []}, \"result\": "
  ++ "{\"con\": {\"qname\": {\"module\": \"std\", \"name\": {\"text\": \"any\"}}}}"
  ++ "}}"
  where
    emitArgType _ =
      "{\"multiplicity\": \"many\", \"type\": "
      ++ "{\"con\": {\"qname\": {\"module\": \"std\", \"name\": {\"text\": \"any\"}}}}}"

-- | Emit an OrganIR expression from a parsed Lambda expression.
emitExpr :: LambdaExpr -> String
emitExpr (LVar name uniq) =
  "{\"evar\": {\"text\": " ++ jsonString name ++ ", \"unique\": " ++ show uniq ++ "}}"
emitExpr (LInt n) =
  "{\"elit\": {\"int\": " ++ show n ++ "}}"
emitExpr (LFloat f) =
  "{\"elit\": {\"float\": " ++ show f ++ "}}"
emitExpr (LString s) =
  "{\"elit\": {\"string\": " ++ jsonString s ++ "}}"
emitExpr (LLam params body) =
  "{\"elam\": {\"params\": ["
  ++ intercalate ", " (map emitParam params)
  ++ "], \"body\": " ++ emitExpr body ++ "}}"
  where
    emitParam p =
      "{\"name\": {\"text\": " ++ jsonString (opName p)
      ++ ", \"unique\": " ++ show (opUnique p) ++ "}"
      ++ ", \"type\": {\"con\": {\"qname\": {\"module\": \"std\", \"name\": {\"text\": \"any\"}}}}}"
emitExpr (LApp fn args) =
  "{\"eapp\": {\"fn\": " ++ emitExpr fn
  ++ ", \"args\": [" ++ intercalate ", " (map emitExpr args) ++ "]}}"
emitExpr (LLet binds body) =
  "{\"elet\": {\"binds\": ["
  ++ intercalate ", " (map emitLetBind binds)
  ++ "], \"body\": " ++ emitExpr body ++ "}}"
emitExpr (LLetRec binds body) =
  -- OrganIR doesn't distinguish let/letrec, use elet
  "{\"elet\": {\"binds\": ["
  ++ intercalate ", " (map emitLetBind binds)
  ++ "], \"body\": " ++ emitExpr body ++ "}}"
emitExpr (LIf cond thn els) =
  "{\"ecase\": {\"scrutinee\": " ++ emitExpr cond
  ++ ", \"branches\": ["
  ++ "{\"pattern\": {\"pat_lit\": {\"bool\": true}}, \"body\": " ++ emitExpr thn ++ "}, "
  ++ "{\"pattern\": {\"pat_lit\": {\"bool\": false}}, \"body\": " ++ emitExpr els ++ "}"
  ++ "]}}"
emitExpr (LSwitch scrut branches) =
  "{\"ecase\": {\"scrutinee\": " ++ emitExpr scrut
  ++ ", \"branches\": ["
  ++ intercalate ", " (map emitSwitchBranch branches)
  ++ "]}}"
  where
    emitSwitchBranch (tag, body) =
      "{\"pattern\": {\"pat_lit\": {\"int\": " ++ show tag ++ "}}, \"body\": " ++ emitExpr body ++ "}"
emitExpr (LMakeBlock tag fields) =
  "{\"econ\": {\"name\": {\"module\": \"ocaml\", \"name\": {\"text\": \"block_"
  ++ show tag ++ "\"}}, \"args\": ["
  ++ intercalate ", " (map emitExpr fields) ++ "]}}"
emitExpr (LPrim name args) =
  "{\"eapp\": {\"fn\": {\"evar\": {\"text\": " ++ jsonString name
  ++ ", \"unique\": 0}}, \"args\": ["
  ++ intercalate ", " (map emitExpr args) ++ "]}}"
emitExpr (LSeq a b) =
  "{\"elet\": {\"binds\": [{\"name\": {\"text\": \"_seq\", \"unique\": 0}, "
  ++ "\"expr\": " ++ emitExpr a ++ "}], \"body\": " ++ emitExpr b ++ "}}"
emitExpr (LRaw s) =
  "{\"elit\": {\"string\": " ++ jsonString s ++ "}}"

emitLetBind :: (String, Int, LambdaExpr) -> String
emitLetBind (name, uniq, expr) =
  "{\"name\": {\"text\": " ++ jsonString name ++ ", \"unique\": " ++ show uniq
  ++ "}, \"expr\": " ++ emitExpr expr ++ "}"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

jsonString :: String -> String
jsonString s = "\"" ++ concatMap escChar s ++ "\""
  where
    escChar '"'  = "\\\""
    escChar '\\' = "\\\\"
    escChar '\n' = "\\n"
    escChar '\t' = "\\t"
    escChar c    = [c]

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

capitalise :: String -> String
capitalise []     = []
capitalise (c:cs) = toUpper' c : cs
  where
    toUpper' x
      | x >= 'a' && x <= 'z' = toEnum (fromEnum x - 32)
      | otherwise             = x
