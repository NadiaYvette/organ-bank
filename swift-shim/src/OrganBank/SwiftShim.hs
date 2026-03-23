-- | Swift SIL Extraction Shim
--
-- Runs @swiftc -emit-sil@ on a Swift source file, parses the canonical SIL
-- text output, and emits OrganIR JSON on stdout.
--
-- SIL (Swift Intermediate Language) is a high-level, SSA-form IR that sits
-- between the AST and LLVM IR in the Swift compiler pipeline. It exposes
-- ARC operations (strong_retain, strong_release) explicitly, which we map
-- to affine multiplicity in OrganIR.

module OrganBank.SwiftShim
  ( extractOrganIR
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.FilePath (takeBaseName)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.Char (isDigit, isAlphaNum)

-- ---------------------------------------------------------------------------
-- Types for parsed SIL
-- ---------------------------------------------------------------------------

data SILFunction = SILFunction
  { silName       :: !Text     -- ^ Mangled name (e.g. "$s4main9factorialyS2iF")
  , silDemangle   :: !Text     -- ^ Best-effort demangled name
  , silAccess     :: !Text     -- ^ "public", "hidden", "private", etc.
  , silConvention :: !Text     -- ^ "thin", "method", "witness_method", etc.
  , silParams     :: ![Text]   -- ^ Parameter types from the SIL type signature
  , silReturn     :: !Text     -- ^ Return type from the SIL type signature
  , silBlocks     :: !Int      -- ^ Number of basic blocks
  , silThrows     :: !Bool     -- ^ Whether the function type includes @error
  , silAsync      :: !Bool     -- ^ Whether the function has @async attribute
  , silARC        :: !Bool     -- ^ Whether the body contains strong_retain/strong_release
  }

-- ---------------------------------------------------------------------------
-- Top-level entry point
-- ---------------------------------------------------------------------------

-- | Extract OrganIR JSON from a Swift source file.
extractOrganIR :: FilePath -> IO (Either Text Text)
extractOrganIR inputPath = do
  (exitCode, silText, stderrText) <-
    readProcessWithExitCode "swiftc" ["-emit-sil", inputPath] ""
  case exitCode of
    ExitSuccess -> do
      let modName = takeBaseName inputPath
          fns     = parseSIL (T.pack silText)
      pure $ Right $ emitOrganIR modName fns
    ExitFailure code ->
      pure $ Left $ T.unlines
        [ "swiftc -emit-sil failed (exit " <> T.pack (show code) <> ")"
        , T.pack stderrText
        ]

-- ---------------------------------------------------------------------------
-- SIL Parser
-- ---------------------------------------------------------------------------

-- | Parse SIL text into a list of function definitions.
-- We scan for lines matching:
--   sil [attributes] @mangled_name : $@convention(...) (Params) -> RetType {
-- and count basic blocks (bb0:, bb1:, ...) within each function body.
parseSIL :: Text -> [SILFunction]
parseSIL silText =
  let ls = zip [0 :: Int ..] (T.lines silText)
      -- Find function header lines
      fnHeaders = [(i, l) | (i, l) <- ls, isFuncHeader l]
      allLines  = T.lines silText
  in map (parseFuncAt allLines) fnHeaders

-- | A SIL function header looks like:
--   sil [serialized] [ossa] @$s... : $@convention(thin) (...) -> ... {
-- or simply:
--   sil hidden @$s... : $@convention(thin) (...) -> ... {
isFuncHeader :: Text -> Bool
isFuncHeader line =
  let stripped = T.stripStart line
  in T.isPrefixOf "sil " stripped
     && T.isInfixOf " @" stripped
     && T.isInfixOf "{" stripped
     && not (T.isPrefixOf "sil_" stripped)  -- skip sil_witness_table, sil_vtable, etc.

-- | Parse a single function starting at the given header line.
parseFuncAt :: [Text] -> (Int, Text) -> SILFunction
parseFuncAt allLines (lineIdx, headerLine) =
  let -- Extract the mangled name: find @name
      mangledName = extractMangledName headerLine
      -- Extract access level
      access = extractAccess headerLine
      -- Extract convention
      convention = extractConvention headerLine
      -- Extract the type signature after ": $"
      typeSig = extractTypeSig headerLine
      -- Parse params and return from type sig
      (params, retTy) = parseTypeSig typeSig
      -- Check for @error (throws)
      throws = T.isInfixOf "@error" typeSig
      -- Check for @async attribute
      async = T.isInfixOf "[async]" headerLine || T.isInfixOf "@async" headerLine
      -- Count basic blocks in the function body
      bodyLines = takeBody (drop (lineIdx + 1) allLines)
      blockCount = length [l | l <- bodyLines, isBasicBlockLabel l]
      -- Check for ARC operations
      hasARC = any isARCInstruction bodyLines
      -- Demangle the name
      demangled = demangleName mangledName
  in SILFunction
    { silName       = mangledName
    , silDemangle   = demangled
    , silAccess     = access
    , silConvention = convention
    , silParams     = params
    , silReturn     = retTy
    , silBlocks     = blockCount
    , silThrows     = throws
    , silAsync      = async
    , silARC        = hasARC
    }

-- | Extract the mangled name from @name in the header.
extractMangledName :: Text -> Text
extractMangledName line =
  case T.breakOn "@" (T.drop 4 line) of  -- skip "sil "
    (_, rest)
      | T.null rest -> "<unknown>"
      | otherwise ->
          let afterAt = T.drop 1 rest  -- skip '@'
              name = T.takeWhile (\c -> c /= ' ' && c /= ':') afterAt
          in name

-- | Extract the access level from the header.
-- SIL access: hidden, shared, public, private (or none = public).
extractAccess :: Text -> Text
extractAccess line
  | T.isInfixOf " hidden " line  = "hidden"
  | T.isInfixOf " shared " line  = "shared"
  | T.isInfixOf " private " line = "private"
  | otherwise                     = "public"

-- | Extract @convention(...) from the header.
extractConvention :: Text -> Text
extractConvention line =
  case T.breakOn "@convention(" line of
    (_, rest)
      | T.null rest -> "thin"
      | otherwise ->
          let afterOpen = T.drop (T.length "@convention(") rest
          in T.takeWhile (/= ')') afterOpen

-- | Extract the type signature portion after ": $" from the header.
extractTypeSig :: Text -> Text
extractTypeSig line =
  case T.breakOn ": $" line of
    (_, rest)
      | T.null rest -> ""
      | otherwise ->
          -- Take everything after ": $" up to " {"
          let sig = T.drop 3 rest  -- skip ": $"
          in case T.breakOn " {" sig of
               (s, _) -> T.strip s

-- | Parse a SIL type signature like:
--   @convention(thin) (Int, Int) -> Int
--   @convention(thin) (@guaranteed String) -> @owned String
--   @convention(thin) (Int) -> (Int, @error any Error)
-- Returns (params, return_type).
parseTypeSig :: Text -> ([Text], Text)
parseTypeSig sig =
  let -- Strip @convention(...) prefix
      stripped = case T.breakOn ") " sig of
                   (_, rest)
                     | T.isPrefixOf "@convention" sig -> T.drop 2 rest  -- skip ") "
                     | otherwise -> sig
      -- Find the outermost (params) -> retType
      (paramPart, retPart) = splitArrow stripped
      params = parseParamList paramPart
      ret = T.strip retPart
  in (params, if T.null ret then "Void" else ret)

-- | Split "(...) -> RetType" into the param portion and return type.
splitArrow :: Text -> (Text, Text)
splitArrow sig
  | T.isPrefixOf "(" sig =
      -- Find matching close paren
      let inner = findMatchingParen (T.drop 1 sig) 1
          afterParen = T.drop (T.length inner + 2) sig  -- +2 for '(' and ')'
          ret = case T.stripPrefix " -> " (T.strip afterParen) of
                  Just r  -> r
                  Nothing -> case T.stripPrefix "-> " (T.strip afterParen) of
                               Just r  -> r
                               Nothing -> ""
      in (inner, ret)
  | otherwise = ("", sig)

-- | Find the content inside matching parens. Input starts after the opening '('.
-- depth is current nesting level.
findMatchingParen :: Text -> Int -> Text
findMatchingParen t depth
  | T.null t       = ""
  | depth == 0     = ""
  | otherwise =
      let c = T.head t
          rest = T.tail t
      in case c of
           '(' -> T.cons c $ findMatchingParen rest (depth + 1)
           ')' | depth == 1 -> ""
               | otherwise  -> T.cons c $ findMatchingParen rest (depth - 1)
           _   -> T.cons c $ findMatchingParen rest depth

-- | Parse a comma-separated parameter list, handling nested parens/generics.
parseParamList :: Text -> [Text]
parseParamList t
  | T.null (T.strip t) = []
  | otherwise = map cleanParam $ splitParams (T.strip t) 0 "" []

splitParams :: Text -> Int -> Text -> [Text] -> [Text]
splitParams t depth acc result
  | T.null t = reverse (acc : result)
  | otherwise =
      let c = T.head t
          rest = T.tail t
      in case c of
           '(' -> splitParams rest (depth + 1) (T.snoc acc c) result
           ')' -> splitParams rest (depth - 1) (T.snoc acc c) result
           '<' -> splitParams rest (depth + 1) (T.snoc acc c) result
           '>' -> splitParams rest (depth - 1) (T.snoc acc c) result
           ',' | depth == 0 -> splitParams rest 0 "" (acc : result)
           _   -> splitParams rest depth (T.snoc acc c) result

-- | Clean a parameter type by stripping SIL ownership annotations.
cleanParam :: Text -> Text
cleanParam p =
  let stripped = T.strip p
      -- Remove ownership qualifiers: @guaranteed, @owned, @in, @inout, @in_guaranteed, etc.
      noOwn = foldl (\s prefix -> maybe s T.strip (T.stripPrefix prefix s))
                stripped
                [ "@guaranteed ", "@owned ", "@in_guaranteed "
                , "@inout ", "@in ", "@out "
                , "@callee_guaranteed ", "@callee_owned "
                ]
  in if T.null noOwn then stripped else noOwn

-- | Collect lines belonging to a function body (until the closing "}")
takeBody :: [Text] -> [Text]
takeBody = go 1  -- already inside opening brace
  where
    go :: Int -> [Text] -> [Text]
    go _ [] = []
    go depth (l:ls)
      | depth <= 0 = []
      | otherwise =
          let opens  = T.count "{" l
              closes = T.count "}" l
              depth' = depth + opens - closes
          in l : go depth' ls

-- | Check if a line is a basic block label: "bb0:", "bb1(%0 : $Int):", etc.
isBasicBlockLabel :: Text -> Bool
isBasicBlockLabel line =
  let stripped = T.stripStart line
  in T.isPrefixOf "bb" stripped
     && case T.unpack (T.drop 2 stripped) of
          (c:_) -> isDigit c
          _     -> False

-- | Check if a line contains an ARC instruction.
isARCInstruction :: Text -> Bool
isARCInstruction line =
  any (`T.isInfixOf` line)
    [ "strong_retain"
    , "strong_release"
    , "unowned_retain"
    , "unowned_release"
    , "copy_value"
    , "destroy_value"
    , "begin_borrow"
    , "end_borrow"
    ]

-- ---------------------------------------------------------------------------
-- Name Demangling (best-effort)
-- ---------------------------------------------------------------------------

-- | Best-effort demangling of Swift mangled names.
-- Swift names are mangled with a $s prefix. We try to extract a readable
-- name from common mangling patterns. For production use, one would call
-- `swift demangle`, but we avoid the subprocess overhead.
--
-- Common patterns:
--   $s4main9factorialyS2iF  -> main.factorial
--   $s4mainAAyyF             -> main.main (entry thunk)
--
-- The mangling scheme encodes: $s<modLen><mod><nameLen><name>...
demangleName :: Text -> Text
demangleName name
  | T.isPrefixOf "$s" name =
      case decodeComponents (T.drop 2 name) of
        (mod_, rest) ->
          case decodeComponents rest of
            (func, _) | not (T.null func) -> mod_ <> "." <> func
            _ -> mod_ <> "." <> T.takeWhile isAlphaNum rest
  | T.isPrefixOf "@$s" name = demangleName (T.drop 1 name)
  | T.isPrefixOf "$S" name  = demangleName ("$s" <> T.drop 2 name)
  | otherwise = name
  where
    -- Decode a length-prefixed component: "4main..." -> ("main", "...")
    decodeComponents :: Text -> (Text, Text)
    decodeComponents t =
      let digits = T.takeWhile isDigit t
          rest   = T.drop (T.length digits) t
      in case reads (T.unpack digits) :: [(Int, String)] of
           [(n, "")] | n > 0 && n <= T.length rest ->
             (T.take n rest, T.drop n rest)
           _ -> ("", t)

-- ---------------------------------------------------------------------------
-- OrganIR JSON Emission
-- ---------------------------------------------------------------------------

-- | Emit the parsed SIL functions as OrganIR JSON.
emitOrganIR :: String -> [SILFunction] -> Text
emitOrganIR modName fns =
  T.unlines
    [ "{"
    , "  \"schema_version\": \"1.0.0\","
    , "  \"metadata\": {"
    , "    \"source_language\": \"swift\","
    , "    \"compiler_version\": \"swiftc\","
    , "    \"shim_version\": \"0.1.0\""
    , "  },"
    , "  \"module\": {"
    , "    \"name\": " <> jsonStr (T.pack modName) <> ","
    , "    \"definitions\": ["
    , T.intercalate ",\n" (zipWith (emitFunc modName) [1..] fns)
    , "    ],"
    , "    \"data_types\": [],"
    , "    \"effect_decls\": []"
    , "  }"
    , "}"
    ]

emitFunc :: String -> Int -> SILFunction -> Text
emitFunc modName uid fn =
  let name      = silDemangle fn
      readName  = if T.isInfixOf "." name
                    then T.drop 1 $ T.dropWhile (/= '.') name
                    else name
      -- Swift ARC -> affine multiplicity
      mult      = if silARC fn then "affine" else "many" :: Text
      -- Build effects list
      effects   = buildEffects fn
      -- Build parameter types
      paramArgs = map (emitParam mult) (silParams fn)
      retType   = silReturn fn
      vis       = if silAccess fn == "public" then "public" else "private" :: Text
  in T.unlines
    [ "      {"
    , "        \"name\": {\"module\": " <> jsonStr (T.pack modName)
        <> ", \"name\": {\"text\": " <> jsonStr readName
        <> ", \"unique\": " <> T.pack (show uid) <> "}},"
    , "        \"type\": {"
    , "          \"fn\": {"
    , "            \"args\": [" <> T.intercalate ", " paramArgs <> "],"
    , "            \"effect\": {\"effects\": [" <> T.intercalate ", " effects <> "]},"
    , "            \"result\": " <> emitType retType
    , "          }"
    , "        },"
    , "        \"expr\": {\"elit\": {\"int\": 0}},"
    , "        \"sort\": \"fun\","
    , "        \"visibility\": " <> jsonStr vis <> ","
    , "        \"_sil_mangled\": " <> jsonStr (silName fn) <> ","
    , "        \"_sil_blocks\": " <> T.pack (show (silBlocks fn))
    , "      }"
    ]

-- | Build the OrganIR effects list for a SIL function.
buildEffects :: SILFunction -> [Text]
buildEffects fn = concat
  [ ["{\"module\": \"std\", \"name\": {\"text\": \"exn\"}}" | silThrows fn]
  , ["{\"module\": \"std\", \"name\": {\"text\": \"async\"}}" | silAsync fn]
  ]

-- | Emit a parameter with multiplicity.
emitParam :: Text -> Text -> Text
emitParam mult paramTy =
  "{\"multiplicity\": " <> jsonStr mult
    <> ", \"type\": " <> emitType paramTy <> "}"

-- | Emit a type. For now, all types map to their SIL name wrapped in a con.
emitType :: Text -> Text
emitType ty =
  let cleaned = cleanTypeName ty
  in "{\"con\": {\"qname\": {\"module\": \"swift\", \"name\": {\"text\": "
       <> jsonStr cleaned <> "}}}}"

-- | Clean up a SIL type name for display.
cleanTypeName :: Text -> Text
cleanTypeName ty
  | T.null ty = "Void"
  | T.isPrefixOf "(" ty && T.isSuffixOf ")" ty && not (T.isInfixOf "," ty) =
      -- Unwrap single-element tuples: (Int) -> Int
      cleanTypeName (T.drop 1 (T.dropEnd 1 ty))
  | otherwise =
      -- Strip @error annotations from return type
      case T.breakOn "@error" ty of
        (before, rest)
          | T.null rest -> T.strip ty
          | otherwise   -> T.strip before

-- | Escape a Haskell text value as a JSON string.
jsonStr :: Text -> Text
jsonStr s = "\"" <> T.concatMap escapeChar s <> "\""
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c    = T.singleton c
