{- | Structural schema validation for OrganIR JSON documents.
Checks that raw JSON has the expected shape (required fields, valid enum
values, correct nesting) before full decoding, producing better error
messages than the generic parse failures.
-}
module OrganIR.Schema (schemaCheck, SchemaError (..)) where

import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Parse (JVal (..))

-- | A schema violation with a JSON-path and description.
data SchemaError = SchemaError
    { sePath :: Text
    -- ^ e.g. "metadata.source_language"
    , seMessage :: Text
    }
    deriving (Show)

-- | Validate the raw JSON structure of an OrganIR document.
schemaCheck :: JVal -> [SchemaError]
schemaCheck jv = case jv of
    JObj fs -> checkTopLevel fs
    _ -> [SchemaError "$" "Top-level value must be a JSON object"]

-- * Top level

checkTopLevel :: [(Text, JVal)] -> [SchemaError]
checkTopLevel fs =
    requiredFields "$" ["schema_version", "metadata", "module"] fs
        <> checkStr "$" "schema_version" fs
        <> maybe [] (checkMetadata "$.metadata") (lookup "metadata" fs)
        <> maybe [] (checkModule "$.module") (lookup "module" fs)
        <> extraFields "$" ["schema_version", "metadata", "module"] fs

-- * Metadata

checkMetadata :: Text -> JVal -> [SchemaError]
checkMetadata path jv = case jv of
    JObj fs ->
        requiredFields path ["source_language", "shim_version"] fs
            <> checkSourceLang path fs
            <> checkStr path "shim_version" fs
            <> checkOptStr path "compiler_version" fs
            <> checkOptStr path "source_file" fs
            <> checkOptStr path "timestamp" fs
            <> extraFields path ["source_language", "compiler_version", "source_file", "shim_version", "timestamp"] fs
    _ -> [SchemaError path "metadata must be a JSON object"]

checkSourceLang :: Text -> [(Text, JVal)] -> [SchemaError]
checkSourceLang path fs = case lookup "source_language" fs of
    Just (JStr s)
        | s `elem` validLangs -> []
        | otherwise ->
            [ SchemaError
                (path <> ".source_language")
                ("invalid source_language: " <> quote s <> "; expected one of: " <> T.intercalate ", " validLangs)
            ]
    Just _ -> [SchemaError (path <> ".source_language") "source_language must be a string"]
    Nothing -> [] -- already reported by requiredFields
  where
    validLangs =
        [ "haskell", "rust", "mercury", "idris2", "lean4", "koka"
        , "ocaml", "swift", "erlang", "purescript", "agda", "fsharp"
        , "scala3", "julia", "zig", "c", "cpp", "fortran", "ada"
        , "sml", "common-lisp", "scheme", "prolog", "lua", "forth"
        ]

-- * Module

checkModule :: Text -> JVal -> [SchemaError]
checkModule path jv = case jv of
    JObj fs ->
        requiredFields path ["name", "definitions"] fs
            <> checkStr path "name" fs
            <> checkOptStrArray path "exports" fs
            <> checkDefinitions (path <> ".definitions") (lookup "definitions" fs)
            <> checkOptArray path "data_types" (checkDataType . arrElem path "data_types") fs
            <> checkOptArray path "effect_decls" (checkEffectDecl . arrElem path "effect_decls") fs
            <> extraFields path ["name", "exports", "definitions", "data_types", "effect_decls"] fs
    _ -> [SchemaError path "module must be a JSON object"]

checkDefinitions :: Text -> Maybe JVal -> [SchemaError]
checkDefinitions path = \case
    Just (JArr xs) -> concatMap (\(i, x) -> checkDefinition (arrElem' path i) x) (zip [0 ..] xs)
    Just _ -> [SchemaError path "definitions must be a JSON array"]
    Nothing -> [] -- already reported by requiredFields

checkDefinition :: Text -> JVal -> [SchemaError]
checkDefinition path jv = case jv of
    JObj fs ->
        requiredFields path ["name", "type", "expr", "sort", "visibility"] fs
            <> checkQName (path <> ".name") (lookup "name" fs)
            <> checkType (path <> ".type") (lookup "type" fs)
            <> checkExpr (path <> ".expr") (lookup "expr" fs)
            <> checkEnum path "sort" ["fun", "val", "external", "con"] fs
            <> checkEnum path "visibility" ["public", "private"] fs
    _ -> [SchemaError path "definition must be a JSON object"]

-- * QName / Name

checkQName :: Text -> Maybe JVal -> [SchemaError]
checkQName _path Nothing = []
checkQName path (Just jv) = case jv of
    JObj fs ->
        requiredFields path ["module", "name"] fs
            <> checkStr path "module" fs
            <> checkName (path <> ".name") (lookup "name" fs)
    _ -> [SchemaError path "qname must be a JSON object"]

checkName :: Text -> Maybe JVal -> [SchemaError]
checkName _path Nothing = []
checkName path (Just jv) = case jv of
    JObj fs ->
        requiredFields path ["text"] fs
            <> checkStr path "text" fs
            <> checkOptInt path "unique" fs
    _ -> [SchemaError path "name must be a JSON object"]

-- * Types

checkType :: Text -> Maybe JVal -> [SchemaError]
checkType _path Nothing = []
checkType path (Just jv) = case jv of
    JObj [("forall", v)] -> checkForallType (path <> ".forall") v
    JObj [("fn", v)] -> checkFnType (path <> ".fn") v
    JObj [("app", v)] -> checkAppType (path <> ".app") v
    JObj [("con", v)] -> checkConType (path <> ".con") v
    JObj [("var", v)] -> checkName (path <> ".var") (Just v)
    JObj [("syn", v)] -> checkSynType (path <> ".syn") v
    JObj fs ->
        [SchemaError path ("type must have exactly one tag key (forall|fn|app|con|var|syn), got: " <> T.intercalate ", " (map fst fs))]
    _ -> [SchemaError path "type must be a JSON object"]

checkForallType :: Text -> JVal -> [SchemaError]
checkForallType path jv = case jv of
    JObj fs ->
        requiredFields path ["vars", "body"] fs
            <> checkTyVarsArray (path <> ".vars") (lookup "vars" fs)
            <> checkType (path <> ".body") (lookup "body" fs)
    _ -> [SchemaError path "forall body must be a JSON object"]

checkFnType :: Text -> JVal -> [SchemaError]
checkFnType path jv = case jv of
    JObj fs ->
        requiredFields path ["args", "effect", "result"] fs
            <> checkFnArgs (path <> ".args") (lookup "args" fs)
            <> checkEffectRow (path <> ".effect") (lookup "effect" fs)
            <> checkType (path <> ".result") (lookup "result" fs)
    _ -> [SchemaError path "fn body must be a JSON object"]

checkFnArgs :: Text -> Maybe JVal -> [SchemaError]
checkFnArgs _path Nothing = []
checkFnArgs path (Just (JArr xs)) =
    concatMap (\(i, x) -> checkFnArg (arrElem' path i) x) (zip [0 ..] xs)
checkFnArgs path _ = [SchemaError path "fn args must be a JSON array"]

checkFnArg :: Text -> JVal -> [SchemaError]
checkFnArg path jv = case jv of
    JObj fs ->
        requiredFields path ["type"] fs
            <> checkType (path <> ".type") (lookup "type" fs)
            <> checkOptEnum path "multiplicity" ["many", "affine", "linear"] fs
    _ -> [SchemaError path "fn arg must be a JSON object"]

checkAppType :: Text -> JVal -> [SchemaError]
checkAppType path jv = case jv of
    JObj fs ->
        requiredFields path ["con", "args"] fs
            <> checkQName (path <> ".con") (lookup "con" fs)
            <> checkTypeArray (path <> ".args") (lookup "args" fs)
    _ -> [SchemaError path "app body must be a JSON object"]

checkConType :: Text -> JVal -> [SchemaError]
checkConType path jv = case jv of
    JObj fs ->
        requiredFields path ["qname"] fs
            <> checkQName (path <> ".qname") (lookup "qname" fs)
    _ -> [SchemaError path "con body must be a JSON object"]

checkSynType :: Text -> JVal -> [SchemaError]
checkSynType path jv = case jv of
    JObj fs ->
        requiredFields path ["name", "expansion"] fs
            <> checkQName (path <> ".name") (lookup "name" fs)
            <> checkType (path <> ".expansion") (lookup "expansion" fs)
    _ -> [SchemaError path "syn body must be a JSON object"]

checkTypeArray :: Text -> Maybe JVal -> [SchemaError]
checkTypeArray _path Nothing = []
checkTypeArray path (Just (JArr xs)) =
    concatMap (\(i, x) -> checkType (arrElem' path i) (Just x)) (zip [0 ..] xs)
checkTypeArray path _ = [SchemaError path "expected a JSON array of types"]

checkTyVarsArray :: Text -> Maybe JVal -> [SchemaError]
checkTyVarsArray _path Nothing = []
checkTyVarsArray path (Just (JArr xs)) =
    concatMap (\(i, x) -> checkTyVar (arrElem' path i) x) (zip [0 ..] xs)
checkTyVarsArray path _ = [SchemaError path "expected a JSON array of type variables"]

checkTyVar :: Text -> JVal -> [SchemaError]
checkTyVar path jv = case jv of
    JObj fs ->
        requiredFields path ["name"] fs
            <> checkName (path <> ".name") (lookup "name" fs)
            <> checkOptStr path "kind" fs
    _ -> [SchemaError path "type variable must be a JSON object"]

checkEffectRow :: Text -> Maybe JVal -> [SchemaError]
checkEffectRow _path Nothing = []
checkEffectRow path (Just jv) = case jv of
    JObj fs ->
        requiredFields path ["effects"] fs
            <> checkQNameArray (path <> ".effects") (lookup "effects" fs)
    _ -> [SchemaError path "effect row must be a JSON object"]

checkQNameArray :: Text -> Maybe JVal -> [SchemaError]
checkQNameArray _path Nothing = []
checkQNameArray path (Just (JArr xs)) =
    concatMap (\(i, x) -> checkQName (arrElem' path i) (Just x)) (zip [0 ..] xs)
checkQNameArray path _ = [SchemaError path "expected a JSON array of qnames"]

-- * Expressions

checkExpr :: Text -> Maybe JVal -> [SchemaError]
checkExpr _path Nothing = []
checkExpr path (Just jv) = case jv of
    JObj [(tag, v)] | tag `elem` validExprTags -> checkExprTag path tag v
    JObj fs ->
        [SchemaError path ("expr must have exactly one tag key, got: " <> T.intercalate ", " (map fst fs))]
    _ -> [SchemaError path "expr must be a JSON object"]
  where
    validExprTags =
        [ "evar", "elit", "econ", "eapp", "elam", "elet", "ecase"
        , "etype_app", "etype_lam", "eperform", "ehandle"
        , "eretain", "erelease", "edrop", "ereuse", "edelay", "eforce"
        ]

checkExprTag :: Text -> Text -> JVal -> [SchemaError]
checkExprTag path tag v = case tag of
    "evar" -> checkName (path <> ".evar") (Just v)
    "elit" -> checkLit (path <> ".elit") v
    "econ" -> checkECon (path <> ".econ") v
    "eapp" -> checkEApp (path <> ".eapp") v
    "elam" -> checkELam (path <> ".elam") v
    "elet" -> checkELet (path <> ".elet") v
    "ecase" -> checkECase (path <> ".ecase") v
    "etype_app" -> checkETypeApp (path <> ".etype_app") v
    "etype_lam" -> checkETypeLam (path <> ".etype_lam") v
    "eperform" -> checkEPerform (path <> ".eperform") v
    "ehandle" -> checkEHandle (path <> ".ehandle") v
    "eretain" -> checkName (path <> ".eretain") (Just v)
    "erelease" -> checkName (path <> ".erelease") (Just v)
    "edrop" -> checkName (path <> ".edrop") (Just v)
    "ereuse" -> checkName (path <> ".ereuse") (Just v)
    "edelay" -> checkExpr (path <> ".edelay") (Just v)
    "eforce" -> checkExpr (path <> ".eforce") (Just v)
    _ -> [SchemaError path ("unknown expr tag: " <> tag)]

checkLit :: Text -> JVal -> [SchemaError]
checkLit path jv = case jv of
    JObj [("int", JInt _)] -> []
    JObj [("float", JFloat _)] -> []
    JObj [("float", JInt _)] -> [] -- integer-valued floats are OK
    JObj [("string", JStr _)] -> []
    JObj [("bool", JBool _)] -> []
    JObj [(k, _)] | k `elem` ["int", "float", "string", "bool"] ->
        [SchemaError path ("literal " <> quote k <> " has wrong value type")]
    JObj fs ->
        [SchemaError path ("literal must have exactly one key (int|float|string|bool), got: " <> T.intercalate ", " (map fst fs))]
    _ -> [SchemaError path "literal must be a JSON object"]

checkECon :: Text -> JVal -> [SchemaError]
checkECon path jv = case jv of
    JObj fs ->
        requiredFields path ["name"] fs
            <> checkQName (path <> ".name") (lookup "name" fs)
            <> checkOptExprArray path "args" fs
    _ -> [SchemaError path "econ must be a JSON object"]

checkEApp :: Text -> JVal -> [SchemaError]
checkEApp path jv = case jv of
    JObj fs ->
        requiredFields path ["fn", "args"] fs
            <> checkExpr (path <> ".fn") (lookup "fn" fs)
            <> checkExprArray (path <> ".args") (lookup "args" fs)
    _ -> [SchemaError path "eapp must be a JSON object"]

checkELam :: Text -> JVal -> [SchemaError]
checkELam path jv = case jv of
    JObj fs ->
        requiredFields path ["params", "body"] fs
            <> checkLamParams (path <> ".params") (lookup "params" fs)
            <> checkExpr (path <> ".body") (lookup "body" fs)
    _ -> [SchemaError path "elam must be a JSON object"]

checkLamParams :: Text -> Maybe JVal -> [SchemaError]
checkLamParams _path Nothing = []
checkLamParams path (Just (JArr xs)) =
    concatMap (\(i, x) -> checkLamParam (arrElem' path i) x) (zip [0 ..] xs)
checkLamParams path _ = [SchemaError path "params must be a JSON array"]

checkLamParam :: Text -> JVal -> [SchemaError]
checkLamParam path jv = case jv of
    JObj fs ->
        requiredFields path ["name"] fs
            <> checkName (path <> ".name") (lookup "name" fs)
    _ -> [SchemaError path "param must be a JSON object"]

checkELet :: Text -> JVal -> [SchemaError]
checkELet path jv = case jv of
    JObj fs ->
        requiredFields path ["binds", "body"] fs
            <> checkLetBinds (path <> ".binds") (lookup "binds" fs)
            <> checkExpr (path <> ".body") (lookup "body" fs)
    _ -> [SchemaError path "elet must be a JSON object"]

checkLetBinds :: Text -> Maybe JVal -> [SchemaError]
checkLetBinds _path Nothing = []
checkLetBinds path (Just (JArr xs)) =
    concatMap (\(i, x) -> checkLetBind (arrElem' path i) x) (zip [0 ..] xs)
checkLetBinds path _ = [SchemaError path "binds must be a JSON array"]

checkLetBind :: Text -> JVal -> [SchemaError]
checkLetBind path jv = case jv of
    JObj fs ->
        requiredFields path ["name", "expr"] fs
            <> checkName (path <> ".name") (lookup "name" fs)
            <> checkExpr (path <> ".expr") (lookup "expr" fs)
    _ -> [SchemaError path "let bind must be a JSON object"]

checkECase :: Text -> JVal -> [SchemaError]
checkECase path jv = case jv of
    JObj fs ->
        requiredFields path ["scrutinee", "branches"] fs
            <> checkExpr (path <> ".scrutinee") (lookup "scrutinee" fs)
            <> checkBranches (path <> ".branches") (lookup "branches" fs)
    _ -> [SchemaError path "ecase must be a JSON object"]

checkBranches :: Text -> Maybe JVal -> [SchemaError]
checkBranches _path Nothing = []
checkBranches path (Just (JArr xs)) =
    concatMap (\(i, x) -> checkBranch (arrElem' path i) x) (zip [0 ..] xs)
checkBranches path _ = [SchemaError path "branches must be a JSON array"]

checkBranch :: Text -> JVal -> [SchemaError]
checkBranch path jv = case jv of
    JObj fs ->
        requiredFields path ["pattern", "body"] fs
            <> checkPattern (path <> ".pattern") (lookup "pattern" fs)
            <> checkExpr (path <> ".body") (lookup "body" fs)
    _ -> [SchemaError path "branch must be a JSON object"]

checkPattern :: Text -> Maybe JVal -> [SchemaError]
checkPattern _path Nothing = []
checkPattern path (Just jv) = case jv of
    JObj [("pat_con", v)] -> case v of
        JObj fs ->
            requiredFields (path <> ".pat_con") ["name"] fs
                <> checkQName (path <> ".pat_con.name") (lookup "name" fs)
        _ -> [SchemaError (path <> ".pat_con") "pat_con must be a JSON object"]
    JObj [("pat_lit", v)] -> checkLit (path <> ".pat_lit") v
    JObj [("pat_var", v)] -> case v of
        JObj fs ->
            requiredFields (path <> ".pat_var") ["name"] fs
                <> checkName (path <> ".pat_var.name") (lookup "name" fs)
        _ -> [SchemaError (path <> ".pat_var") "pat_var must be a JSON object"]
    JObj [("pat_wild", _)] -> []
    JObj fs ->
        [SchemaError path ("pattern must have exactly one tag (pat_con|pat_lit|pat_var|pat_wild), got: " <> T.intercalate ", " (map fst fs))]
    _ -> [SchemaError path "pattern must be a JSON object"]

checkETypeApp :: Text -> JVal -> [SchemaError]
checkETypeApp path jv = case jv of
    JObj fs ->
        requiredFields path ["expr", "types"] fs
            <> checkExpr (path <> ".expr") (lookup "expr" fs)
            <> checkTypeArray (path <> ".types") (lookup "types" fs)
    _ -> [SchemaError path "etype_app must be a JSON object"]

checkETypeLam :: Text -> JVal -> [SchemaError]
checkETypeLam path jv = case jv of
    JObj fs ->
        requiredFields path ["vars", "body"] fs
            <> checkTyVarsArray (path <> ".vars") (lookup "vars" fs)
            <> checkExpr (path <> ".body") (lookup "body" fs)
    _ -> [SchemaError path "etype_lam must be a JSON object"]

checkEPerform :: Text -> JVal -> [SchemaError]
checkEPerform path jv = case jv of
    JObj fs ->
        requiredFields path ["effect", "op"] fs
            <> checkQName (path <> ".effect") (lookup "effect" fs)
            <> checkStr path "op" fs
            <> checkOptExprArray path "args" fs
    _ -> [SchemaError path "eperform must be a JSON object"]

checkEHandle :: Text -> JVal -> [SchemaError]
checkEHandle path jv = case jv of
    JObj fs ->
        requiredFields path ["effect", "body", "handler"] fs
            <> checkQName (path <> ".effect") (lookup "effect" fs)
            <> checkExpr (path <> ".body") (lookup "body" fs)
            <> checkExpr (path <> ".handler") (lookup "handler" fs)
    _ -> [SchemaError path "ehandle must be a JSON object"]

checkExprArray :: Text -> Maybe JVal -> [SchemaError]
checkExprArray _path Nothing = []
checkExprArray path (Just (JArr xs)) =
    concatMap (\(i, x) -> checkExpr (arrElem' path i) (Just x)) (zip [0 ..] xs)
checkExprArray path _ = [SchemaError path "expected a JSON array of expressions"]

checkOptExprArray :: Text -> Text -> [(Text, JVal)] -> [SchemaError]
checkOptExprArray path key fs = case lookup key fs of
    Just (JArr xs) -> concatMap (\(i, x) -> checkExpr (arrElem' (path <> "." <> key) i) (Just x)) (zip [0 ..] xs)
    Just _ -> [SchemaError (path <> "." <> key) "expected a JSON array"]
    Nothing -> []

-- * Data types

checkDataType :: Text -> JVal -> [SchemaError]
checkDataType path jv = case jv of
    JObj fs ->
        requiredFields path ["name", "constructors"] fs
            <> checkQName (path <> ".name") (lookup "name" fs)
            <> checkConstructors (path <> ".constructors") (lookup "constructors" fs)
    _ -> [SchemaError path "data_type must be a JSON object"]

checkConstructors :: Text -> Maybe JVal -> [SchemaError]
checkConstructors _path Nothing = []
checkConstructors path (Just (JArr xs)) =
    concatMap (\(i, x) -> checkConstructor (arrElem' path i) x) (zip [0 ..] xs)
checkConstructors path _ = [SchemaError path "constructors must be a JSON array"]

checkConstructor :: Text -> JVal -> [SchemaError]
checkConstructor path jv = case jv of
    JObj fs ->
        requiredFields path ["name", "fields"] fs
            <> checkQName (path <> ".name") (lookup "name" fs)
            <> checkTypeArray (path <> ".fields") (lookup "fields" fs)
    _ -> [SchemaError path "constructor must be a JSON object"]

-- * Effect declarations

checkEffectDecl :: Text -> JVal -> [SchemaError]
checkEffectDecl path jv = case jv of
    JObj fs ->
        requiredFields path ["name", "operations"] fs
            <> checkQName (path <> ".name") (lookup "name" fs)
            <> checkOperations (path <> ".operations") (lookup "operations" fs)
    _ -> [SchemaError path "effect_decl must be a JSON object"]

checkOperations :: Text -> Maybe JVal -> [SchemaError]
checkOperations _path Nothing = []
checkOperations path (Just (JArr xs)) =
    concatMap (\(i, x) -> checkOperation (arrElem' path i) x) (zip [0 ..] xs)
checkOperations path _ = [SchemaError path "operations must be a JSON array"]

checkOperation :: Text -> JVal -> [SchemaError]
checkOperation path jv = case jv of
    JObj fs ->
        requiredFields path ["name", "type"] fs
            <> checkStr path "name" fs
            <> checkType (path <> ".type") (lookup "type" fs)
    _ -> [SchemaError path "operation must be a JSON object"]

-- * Utility helpers

requiredFields :: Text -> [Text] -> [(Text, JVal)] -> [SchemaError]
requiredFields path required fs =
    [ SchemaError (path <> "." <> k) ("required field " <> quote k <> " is missing")
    | k <- required
    , Nothing <- [lookup k fs]
    ]

extraFields :: Text -> [Text] -> [(Text, JVal)] -> [SchemaError]
extraFields path allowed fs =
    [ SchemaError (path <> "." <> k) ("unexpected field " <> quote k)
    | (k, _) <- fs
    , k `notElem` allowed
    ]

checkStr :: Text -> Text -> [(Text, JVal)] -> [SchemaError]
checkStr path key fs = case lookup key fs of
    Just (JStr _) -> []
    Just _ -> [SchemaError (path <> "." <> key) (quote key <> " must be a string")]
    Nothing -> [] -- handled by requiredFields

checkOptStr :: Text -> Text -> [(Text, JVal)] -> [SchemaError]
checkOptStr path key fs = case lookup key fs of
    Just (JStr _) -> []
    Just _ -> [SchemaError (path <> "." <> key) (quote key <> " must be a string")]
    Nothing -> []

checkOptInt :: Text -> Text -> [(Text, JVal)] -> [SchemaError]
checkOptInt path key fs = case lookup key fs of
    Just (JInt _) -> []
    Just _ -> [SchemaError (path <> "." <> key) (quote key <> " must be an integer")]
    Nothing -> []

checkOptStrArray :: Text -> Text -> [(Text, JVal)] -> [SchemaError]
checkOptStrArray path key fs = case lookup key fs of
    Just (JArr xs) ->
        [ SchemaError (arrElem' (path <> "." <> key) i) "expected a string"
        | (i, x) <- zip [0 ..] xs
        , not (isStr x)
        ]
    Just _ -> [SchemaError (path <> "." <> key) (quote key <> " must be a JSON array")]
    Nothing -> []
  where
    isStr (JStr _) = True
    isStr _ = False

checkEnum :: Text -> Text -> [Text] -> [(Text, JVal)] -> [SchemaError]
checkEnum path key valid fs = case lookup key fs of
    Just (JStr s)
        | s `elem` valid -> []
        | otherwise ->
            [ SchemaError
                (path <> "." <> key)
                ("invalid " <> key <> ": " <> quote s <> "; expected one of: " <> T.intercalate ", " valid)
            ]
    Just _ -> [SchemaError (path <> "." <> key) (key <> " must be a string")]
    Nothing -> [] -- handled by requiredFields

checkOptEnum :: Text -> Text -> [Text] -> [(Text, JVal)] -> [SchemaError]
checkOptEnum path key valid fs = case lookup key fs of
    Just (JStr s)
        | s `elem` valid -> []
        | otherwise ->
            [ SchemaError
                (path <> "." <> key)
                ("invalid " <> key <> ": " <> quote s <> "; expected one of: " <> T.intercalate ", " valid)
            ]
    Just _ -> [SchemaError (path <> "." <> key) (key <> " must be a string")]
    Nothing -> []

checkOptArray :: Text -> Text -> (Int -> JVal -> [SchemaError]) -> [(Text, JVal)] -> [SchemaError]
checkOptArray path key checkElem fs = case lookup key fs of
    Just (JArr xs) -> concatMap (\(i, x) -> checkElem i x) (zip [0 ..] xs)
    Just _ -> [SchemaError (path <> "." <> key) (quote key <> " must be a JSON array")]
    Nothing -> []

arrElem :: Text -> Text -> Int -> Text
arrElem path key i = path <> "." <> key <> "[" <> T.pack (show i) <> "]"

arrElem' :: Text -> Int -> Text
arrElem' path i = path <> "[" <> T.pack (show i) <> "]"

quote :: Text -> Text
quote s = "\"" <> s <> "\""
