{- | Hand-rolled JSON parser for OrganIR.
Inverts every @render*@ function in "OrganIR.Json" to produce
OrganIR types from JSON text, with no dependencies beyond base and text.
-}
module OrganIR.Parse (parseOrganIR, JVal (..), parseJSON, maxNestingDepth) where

import Data.Char (chr, digitToInt, isDigit, isHexDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Types

-- * Generic JSON value type

data JVal
    = JObj [(Text, JVal)]
    | JArr [JVal]
    | JStr Text
    | JInt Integer
    | JFloat Double
    | JBool Bool
    | JNull
    deriving (Show)

-- * Generic JSON parser

type P a = Text -> Either Text (a, Text)

-- | Maximum nesting depth for JSON parsing to prevent stack overflow.
maxNestingDepth :: Int
maxNestingDepth = 200

parseJSON :: Text -> Either Text JVal
parseJSON t = do
    (v, rest) <- pValueD 0 (skip t)
    let rest' = skip rest
    if T.null rest'
        then Right v
        else Left $ "Trailing content: " <> T.take 40 rest'

-- | Parse a JSON value with depth tracking.
pValueD :: Int -> P JVal
pValueD depth t
    | depth > maxNestingDepth = Left "Nesting too deep (> 200 levels)"
    | T.null t = Left "Unexpected end of input"
    | otherwise = case T.head t of
        '"' -> do (s, r) <- pString t; Right (JStr s, r)
        '{' -> do (o, r) <- pObjectD (depth + 1) (skip (T.tail t)); Right (JObj o, r)
        '[' -> do (a, r) <- pArrayD (depth + 1) (skip (T.tail t)); Right (JArr a, r)
        't' -> pLit "true" (JBool True) t
        'f' -> pLit "false" (JBool False) t
        'n' -> pLit "null" JNull t
        c | c == '-' || isDigit c -> pNumber t
        _ -> Left $ "Unexpected character: " <> T.take 1 t

-- | Backward-compatible wrapper without depth tracking.
pValue :: P JVal
pValue = pValueD 0

pLit :: Text -> JVal -> P JVal
pLit expected val t
    | expected `T.isPrefixOf` t = Right (val, T.drop (T.length expected) t)
    | otherwise = Left $ "Expected " <> expected

pString :: P Text
pString t = case T.uncons t of
    Just ('"', rest) -> pStrBody "" rest
    _ -> Left "Expected '\"'"

pStrBody :: Text -> P Text
pStrBody acc t = case T.break (\c -> c == '"' || c == '\\') t of
    (chunk, rest)
        | T.null rest -> Left "Unterminated string"
        | T.head rest == '"' -> Right (acc <> chunk, T.tail rest)
        | otherwise -> case T.uncons (T.tail rest) of
            Nothing -> Left "Unterminated string escape"
            Just (c, rest') -> case c of
                'n' -> pStrBody (acc <> chunk <> "\n") rest'
                't' -> pStrBody (acc <> chunk <> "\t") rest'
                'r' -> pStrBody (acc <> chunk <> "\r") rest'
                '\\' -> pStrBody (acc <> chunk <> "\\") rest'
                '"' -> pStrBody (acc <> chunk <> "\"") rest'
                '/' -> pStrBody (acc <> chunk <> "/") rest'
                'b' -> pStrBody (acc <> chunk <> "\b") rest'
                'f' -> pStrBody (acc <> chunk <> "\f") rest'
                'u' ->
                    let (hex, rest'') = T.splitAt 4 rest'
                     in if T.length hex == 4 && T.all isHexDigit hex
                            then pStrBody (acc <> chunk <> T.singleton (chr (hexToInt hex))) rest''
                            else Left "Invalid \\uXXXX escape"
                _ -> Left $ "Unknown escape: \\" <> T.singleton c

hexToInt :: Text -> Int
hexToInt = T.foldl' (\a c -> a * 16 + digitToInt c) 0

pNumber :: P JVal
pNumber t =
    let (numStr, rest) = T.span (\c -> isDigit c || c == '.' || c == '-' || c == '+' || c == 'e' || c == 'E') t
     in if T.any (\c -> c == '.' || c == 'e' || c == 'E') numStr
            then case reads (T.unpack numStr) of
                [(d, "")] -> Right (JFloat d, rest)
                _ -> Left $ "Bad number: " <> numStr
            else case reads (T.unpack numStr) of
                [(n, "")] -> Right (JInt n, rest)
                _ -> Left $ "Bad number: " <> numStr

pObject :: P [(Text, JVal)]
pObject = pObjectD 0

pObjectD :: Int -> P [(Text, JVal)]
pObjectD depth t
    | not (T.null t) && T.head t == '}' = Right ([], T.tail t)
    | otherwise = pObjEntriesD depth t

pObjEntries :: P [(Text, JVal)]
pObjEntries = pObjEntriesD 0

pObjEntriesD :: Int -> P [(Text, JVal)]
pObjEntriesD depth t = do
    (k, rest) <- pString (skip t)
    rest' <- pColon (skip rest)
    (v, rest'') <- pValueD depth (skip rest')
    let rest''' = skip rest''
    case T.uncons rest''' of
        Just (',', more) -> do
            (pairs, final) <- pObjEntriesD depth (skip more)
            Right ((k, v) : pairs, final)
        Just ('}', more) -> Right ([(k, v)], more)
        _ -> Left "Expected ',' or '}' in object"

pColon :: Text -> Either Text Text
pColon t = case T.uncons t of
    Just (':', rest) -> Right rest
    _ -> Left "Expected ':'"

pArray :: P [JVal]
pArray = pArrayD 0

pArrayD :: Int -> P [JVal]
pArrayD depth t
    | not (T.null t) && T.head t == ']' = Right ([], T.tail t)
    | otherwise = pArrElemsD depth t

pArrElems :: P [JVal]
pArrElems = pArrElemsD 0

pArrElemsD :: Int -> P [JVal]
pArrElemsD depth t = do
    (v, rest) <- pValueD depth (skip t)
    let rest' = skip rest
    case T.uncons rest' of
        Just (',', more) -> do
            (vs, final) <- pArrElemsD depth (skip more)
            Right (v : vs, final)
        Just (']', more) -> Right ([v], more)
        _ -> Left "Expected ',' or ']' in array"

skip :: Text -> Text
skip = T.dropWhile isSpace

-- * Helpers for decoding JVal → OrganIR types

type D a = JVal -> Either Text a

field :: Text -> [(Text, JVal)] -> Either Text JVal
field k fs = case lookup k fs of
    Just v -> Right v
    Nothing -> Left $ "Missing field: " <> k

optField :: Text -> [(Text, JVal)] -> Maybe JVal
optField = lookup

asObj :: D [(Text, JVal)]
asObj (JObj fs) = Right fs
asObj v = Left $ "Expected object, got: " <> T.take 40 (T.pack (show v))

asArr :: D [JVal]
asArr (JArr xs) = Right xs
asArr v = Left $ "Expected array, got: " <> T.take 40 (T.pack (show v))

asStr :: D Text
asStr (JStr s) = Right s
asStr v = Left $ "Expected string, got: " <> T.take 40 (T.pack (show v))

asInteger :: D Integer
asInteger (JInt n) = Right n
asInteger (JFloat d) = Right (round d)
asInteger v = Left $ "Expected integer, got: " <> T.take 40 (T.pack (show v))

asInt :: D Int
asInt v = fromIntegral <$> asInteger v

asBool :: D Bool
asBool (JBool b) = Right b
asBool v = Left $ "Expected bool, got: " <> T.take 40 (T.pack (show v))

-- * OrganIR decoders

-- | Parse OrganIR JSON text into the OrganIR type.
parseOrganIR :: Text -> Either Text OrganIR
parseOrganIR t = do
    jv <- parseJSON t
    decodeOrganIR jv

decodeOrganIR :: D OrganIR
decodeOrganIR jv = do
    fs <- asObj jv
    meta <- field "metadata" fs >>= decodeMetadata
    modul <- field "module" fs >>= decodeModule
    Right (OrganIR meta modul)

decodeMetadata :: D Metadata
decodeMetadata jv = do
    fs <- asObj jv
    lang <- field "source_language" fs >>= asStr >>= decodeSourceLang
    let compVer = optField "compiler_version" fs >>= jStr
    let srcFile = optField "source_file" fs >>= jStr
    shimVer <- field "shim_version" fs >>= asStr
    let ts = optField "timestamp" fs >>= jStr
    Right (Metadata lang compVer srcFile shimVer ts)
  where
    jStr (JStr s) = Just s
    jStr _ = Nothing

decodeModule :: D Module
decodeModule jv = do
    fs <- asObj jv
    nm <- field "name" fs >>= asStr
    exports <- case optField "exports" fs of
        Just v -> asArr v >>= mapM asStr
        Nothing -> Right []
    imports <- case optField "imports" fs of
        Just v -> asArr v >>= mapM decodeQName
        Nothing -> Right []
    defs <- field "definitions" fs >>= asArr >>= mapM decodeDef
    dts <- case optField "data_types" fs of
        Just v -> asArr v >>= mapM decodeDataType
        Nothing -> Right []
    eds <- case optField "effect_decls" fs of
        Just v -> asArr v >>= mapM decodeEffectDecl
        Nothing -> Right []
    Right (Module nm exports imports defs dts eds)

decodeDef :: D Definition
decodeDef jv = do
    fs <- asObj jv
    qn <- field "name" fs >>= decodeQName
    ty <- field "type" fs >>= decodeTy
    expr <- field "expr" fs >>= decodeExpr
    sort <- field "sort" fs >>= asStr >>= decodeSort
    vis <- field "visibility" fs >>= asStr >>= decodeVisibility
    arity <- case optField "arity" fs of
        Just v -> asInt v
        Nothing -> Right 0
    Right (Definition qn ty expr sort vis arity)

decodeQName :: D QName
decodeQName jv = do
    fs <- asObj jv
    m <- field "module" fs >>= asStr
    n <- field "name" fs >>= decodeName
    Right (QName m n)

decodeName :: D Name
decodeName jv = do
    fs <- asObj jv
    t <- field "text" fs >>= asStr
    u <- field "unique" fs >>= asInt
    Right (Name t u)

decodeTy :: D Ty
decodeTy jv = do
    fs <- asObj jv
    case fs of
        [("forall", v)] -> do
            inner <- asObj v
            vars <- field "vars" inner >>= asArr >>= mapM decodeTyVar
            body <- field "body" inner >>= decodeTy
            Right (TForall vars body)
        [("fn", v)] -> do
            inner <- asObj v
            args <- field "args" inner >>= asArr >>= mapM decodeFnArg
            eff <- field "effect" inner >>= decodeEffectRow
            result <- field "result" inner >>= decodeTy
            Right (TFn args eff result)
        [("app", v)] -> do
            inner <- asObj v
            con <- field "con" inner >>= decodeQName
            args <- field "args" inner >>= asArr >>= mapM decodeTy
            Right (TApp con args)
        [("con", v)] -> do
            inner <- asObj v
            qn <- field "qname" inner >>= decodeQName
            -- Recognize TAny: module "" name "any" unique 0
            if qn == QName "" (Name "any" 0) then Right TAny else Right (TCon qn)
        [("var", v)] -> TVar <$> decodeName v
        [("syn", v)] -> do
            inner <- asObj v
            n <- field "name" inner >>= decodeQName
            expansion <- field "expansion" inner >>= decodeTy
            Right (TSyn n expansion)
        _ -> Left $ "Unknown type tag: " <> T.pack (show (map fst fs))

decodeFnArg :: D FnArg
decodeFnArg jv = do
    fs <- asObj jv
    mult <- case optField "multiplicity" fs of
        Just v -> Just <$> (asStr v >>= decodeMultiplicity)
        Nothing -> Right Nothing
    ty <- field "type" fs >>= decodeTy
    Right (FnArg mult ty)

decodeTyVar :: D TyVar
decodeTyVar jv = do
    fs <- asObj jv
    n <- field "name" fs >>= decodeName
    let kind = optField "kind" fs >>= \case JStr s -> Just s; _ -> Nothing
    Right (TyVar n kind)

decodeEffectRow :: D EffectRow
decodeEffectRow jv = do
    fs <- asObj jv
    effs <- field "effects" fs >>= asArr >>= mapM decodeQName
    let tail_ =
            optField "tail" fs >>= \v -> case v of
                JObj _ -> case decodeName v of Right n -> Just n; _ -> Nothing
                _ -> Nothing
    Right (EffectRow effs tail_)

decodeExpr :: D Expr
decodeExpr jv = do
    fs <- asObj jv
    case fs of
        [("evar", v)] -> do
            n <- decodeName v
            -- Recognize EUnreachable: _unreachable/0
            if n == Name "_unreachable" 0 then Right EUnreachable else Right (EVar n)
        [("elit", v)] -> ELit <$> decodeLit v
        [("econ", v)] -> do
            inner <- asObj v
            qn <- field "name" inner >>= decodeQName
            args <- case optField "args" inner of
                Just a -> asArr a >>= mapM decodeExpr
                Nothing -> Right []
            -- Recognize ETuple and EList
            case qn of
                QName "" (Name "tuple" 0) -> Right (ETuple args)
                QName "" (Name "list" 0) -> Right (EList args)
                _ -> Right (ECon qn args)
        [("eapp", v)] -> do
            inner <- asObj v
            fn <- field "fn" inner >>= decodeExpr
            args <- field "args" inner >>= asArr >>= mapM decodeExpr
            Right (EApp fn args)
        [("elam", v)] -> do
            inner <- asObj v
            params <- field "params" inner >>= asArr >>= mapM decodeLamParam
            body <- field "body" inner >>= decodeExpr
            Right (ELam params body)
        [("elet", v)] -> do
            inner <- asObj v
            binds <- field "binds" inner >>= asArr >>= mapM decodeLetBind
            body <- field "body" inner >>= decodeExpr
            Right (ELet binds body)
        [("ecase", v)] -> do
            inner <- asObj v
            scrut <- field "scrutinee" inner >>= decodeExpr
            branches <- field "branches" inner >>= asArr >>= mapM decodeBranch
            Right (ECase scrut branches)
        [("etype_app", v)] -> do
            inner <- asObj v
            e <- field "expr" inner >>= decodeExpr
            tys <- field "types" inner >>= asArr >>= mapM decodeTy
            Right (ETypeApp e tys)
        [("etype_lam", v)] -> do
            inner <- asObj v
            vars <- field "vars" inner >>= asArr >>= mapM decodeTyVar
            body <- field "body" inner >>= decodeExpr
            Right (ETypeLam vars body)
        [("eperform", v)] -> do
            inner <- asObj v
            eff <- field "effect" inner >>= decodeQName
            op <- field "op" inner >>= asStr
            args <- case optField "args" inner of
                Just a -> asArr a >>= mapM decodeExpr
                Nothing -> Right []
            -- Recognize ERaise: effect exn, op "raise"
            case (eff, op, args) of
                (QName "" (Name "exn" 0), "raise", [arg]) -> Right (ERaise arg)
                _ -> Right (EPerform eff op args)
        [("ehandle", v)] -> do
            inner <- asObj v
            eff <- field "effect" inner >>= decodeQName
            body <- field "body" inner >>= decodeExpr
            handler <- field "handler" inner >>= decodeExpr
            Right (EHandle eff body handler)
        [("eretain", v)] -> ERetain <$> decodeName v
        [("erelease", v)] -> ERelease <$> decodeName v
        [("edrop", v)] -> EDrop <$> decodeName v
        [("ereuse", v)] -> EReuse <$> decodeName v
        [("edelay", v)] -> EDelay <$> decodeExpr v
        [("eforce", v)] -> EForce <$> decodeExpr v
        _ -> Left $ "Unknown expr tag: " <> T.pack (show (map fst fs))

decodeLit :: D Lit
decodeLit jv = do
    fs <- asObj jv
    case fs of
        [("int", v)] -> LitInt <$> asInteger v
        [("float", v)] -> case v of
            JFloat d -> Right (LitFloat d)
            JInt n -> Right (LitFloat (fromIntegral n))
            JStr s -> case reads (T.unpack s) of
                [(d, "")] -> Right (LitFloat d)
                _ -> Left $ "Bad float: " <> s
            _ -> Left "Expected float value"
        [("string", v)] -> LitString <$> asStr v
        [("bool", v)] -> LitBool <$> asBool v
        _ -> Left $ "Unknown literal tag: " <> T.pack (show (map fst fs))

decodeLamParam :: D LamParam
decodeLamParam jv = do
    fs <- asObj jv
    n <- field "name" fs >>= decodeName
    mty <- case optField "type" fs of
        Just v -> Just <$> decodeTy v
        Nothing -> Right Nothing
    Right (LamParam n mty)

decodeLetBind :: D LetBind
decodeLetBind jv = do
    fs <- asObj jv
    n <- field "name" fs >>= decodeName
    mty <- case optField "type" fs of
        Just v -> Just <$> decodeTy v
        Nothing -> Right Nothing
    e <- field "expr" fs >>= decodeExpr
    Right (LetBind n mty e)

decodeBranch :: D Branch
decodeBranch jv = do
    fs <- asObj jv
    pat <- field "pattern" fs >>= decodePat
    body <- field "body" fs >>= decodeExpr
    Right (Branch pat body)

decodePat :: D Pat
decodePat jv = do
    fs <- asObj jv
    case fs of
        [("pat_con", v)] -> do
            inner <- asObj v
            qn <- field "name" inner >>= decodeQName
            binders <- case optField "args" inner of
                Just a -> asArr a >>= mapM decodePatBinder
                Nothing -> Right []
            Right (PatCon qn binders)
        [("pat_lit", v)] -> PatLit <$> decodeLit v
        [("pat_var", v)] -> do
            inner <- asObj v
            n <- field "name" inner >>= decodeName
            mty <- case optField "type" inner of
                Just tv -> Just <$> decodeTy tv
                Nothing -> Right Nothing
            Right (PatVar n mty)
        [("pat_wild", _)] -> Right PatWild
        _ -> Left $ "Unknown pattern tag: " <> T.pack (show (map fst fs))

decodePatBinder :: D PatBinder
decodePatBinder jv = do
    fs <- asObj jv
    n <- field "name" fs >>= decodeName
    mty <- case optField "type" fs of
        Just v -> Just <$> decodeTy v
        Nothing -> Right Nothing
    Right (PatBinder n mty)

decodeDataType :: D DataType
decodeDataType jv = do
    fs <- asObj jv
    qn <- field "name" fs >>= decodeQName
    tparams <- case optField "type_params" fs of
        Just v -> asArr v >>= mapM decodeTyVar
        Nothing -> Right []
    ctors <- field "constructors" fs >>= asArr >>= mapM decodeConstructor
    Right (DataType qn tparams ctors)

decodeConstructor :: D Constructor
decodeConstructor jv = do
    fs <- asObj jv
    qn <- field "name" fs >>= decodeQName
    fields <- field "fields" fs >>= asArr >>= mapM decodeTy
    Right (Constructor qn fields)

decodeEffectDecl :: D EffectDecl
decodeEffectDecl jv = do
    fs <- asObj jv
    qn <- field "name" fs >>= decodeQName
    tparams <- case optField "type_params" fs of
        Just v -> asArr v >>= mapM decodeTyVar
        Nothing -> Right []
    ops <- field "operations" fs >>= asArr >>= mapM decodeOperation
    Right (EffectDecl qn tparams ops)

decodeOperation :: D Operation
decodeOperation jv = do
    fs <- asObj jv
    n <- field "name" fs >>= asStr
    ty <- field "type" fs >>= decodeTy
    Right (Operation n ty)

-- * Enum decoders

decodeSourceLang :: Text -> Either Text SourceLang
decodeSourceLang = \case
    "haskell" -> Right LHaskell
    "rust" -> Right LRust
    "mercury" -> Right LMercury
    "idris2" -> Right LIdris2
    "lean4" -> Right LLean4
    "koka" -> Right LKoka
    "ocaml" -> Right LOCaml
    "swift" -> Right LSwift
    "erlang" -> Right LErlang
    "purescript" -> Right LPurescript
    "agda" -> Right LAgda
    "fsharp" -> Right LFSharp
    "scala3" -> Right LScala3
    "julia" -> Right LJulia
    "zig" -> Right LZig
    "c" -> Right LC
    "cpp" -> Right LCpp
    "fortran" -> Right LFortran
    "ada" -> Right LAda
    "sml" -> Right LSml
    "common-lisp" -> Right LCommonLisp
    "scheme" -> Right LScheme
    "prolog" -> Right LProlog
    "lua" -> Right LLua
    "forth" -> Right LForth
    t -> Left $ "Unknown source language: " <> t

decodeSort :: Text -> Either Text Sort
decodeSort = \case
    "fun" -> Right SFun
    "val" -> Right SVal
    "external" -> Right SExternal
    "con" -> Right SCon
    t -> Left $ "Unknown sort: " <> t

decodeVisibility :: Text -> Either Text Visibility
decodeVisibility = \case
    "public" -> Right Public
    "private" -> Right Private
    t -> Left $ "Unknown visibility: " <> t

decodeMultiplicity :: Text -> Either Text Multiplicity
decodeMultiplicity = \case
    "many" -> Right Many
    "affine" -> Right Affine
    "linear" -> Right Linear
    t -> Left $ "Unknown multiplicity: " <> t
