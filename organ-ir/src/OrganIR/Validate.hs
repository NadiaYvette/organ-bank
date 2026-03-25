-- | Validation of OrganIR documents: structural checks, placeholder detection.
module OrganIR.Validate (validateOrganIR, Warning (..), Severity (..)) where

import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Types

-- | Severity level for validation messages.
data Severity = Info | Warn | Error
    deriving (Eq, Show)

-- | A validation warning with severity, path, and message.
data Warning = Warning
    { wSeverity :: Severity
    , wPath :: Text
    -- ^ e.g. "module.definitions[2].expr"
    , wMessage :: Text
    }
    deriving (Show)

-- | Validate an OrganIR document, returning all warnings/errors found.
validateOrganIR :: OrganIR -> [Warning]
validateOrganIR ir =
    validateMetadata (irMetadata ir)
        <> validateModule (irModule ir)

-- * Metadata

validateMetadata :: Metadata -> [Warning]
validateMetadata m
    | T.null (metaShimVersion m) =
        [Warning Error "metadata.shimVersion" "shim version is empty"]
    | otherwise = []

-- * Module

validateModule :: Module -> [Warning]
validateModule m =
    [Warning Error "module.name" "module name is empty" | T.null (modName m)]
        <> validateImports m
        <> concatMap (\(i, d) -> validateDef (defPath i) d) (zip [(0 :: Int) ..] (modDefs m))
        <> concatMap (\(i, dt) -> validateDataType (dtPath i) dt) (zip [(0 :: Int) ..] (modDataTypes m))
  where
    defPath i = "module.definitions[" <> T.pack (show i) <> "]"
    dtPath i = "module.dataTypes[" <> T.pack (show i) <> "]"

-- | Check that imported names do not shadow local definitions.
validateImports :: Module -> [Warning]
validateImports m =
    let localNames = map (nameText . qnName . defName) (modDefs m)
        importNames = map (nameText . qnName) (modImports m)
        shadows = filter (`elem` localNames) importNames
     in map
            ( \n ->
                Warning
                    Warn
                    "module.imports"
                    ("imported name " <> quote n <> " shadows a local definition")
            )
            shadows
  where
    quote s = "\"" <> s <> "\""

-- * Definitions

validateDef :: Text -> Definition -> [Warning]
validateDef path def =
    nameChecks
        <> arityChecks
        <> sortChecks
        <> placeholderChecks
        <> validateExpr (path <> ".expr") (defExpr def)
        <> validateTy (path <> ".type") (defType def)
  where
    n = nameText (qnName (defName def))

    nameChecks
        | T.null n = [Warning Error (path <> ".name") "definition name is empty"]
        | otherwise = []

    arityChecks =
        [Warning Error (path <> ".arity") "arity is negative" | defArity def < 0]
            <> arityTypeCheck

    arityTypeCheck = case defType def of
        TFn args _ _ ->
            [ Warning
                Warn
                (path <> ".arity")
                ( "arity ("
                    <> T.pack (show (defArity def))
                    <> ") does not match TFn arg count ("
                    <> T.pack (show (length args))
                    <> ")"
                )
            | defArity def /= length args
            ]
        _ -> []

    sortChecks = case defSort def of
        SFun
            | defArity def == 0 ->
                [Warning Warn (path <> ".sort") "sort is SFun but arity is 0"]
        SVal
            | defArity def /= 0 ->
                [ Warning
                    Warn
                    (path <> ".sort")
                    ("sort is SVal but arity is " <> T.pack (show (defArity def)))
                ]
        _ -> []

    placeholderChecks = case defExpr def of
        ELit (LitInt 0) ->
            [Warning Warn (path <> ".expr") "expression is LitInt 0 (likely placeholder)"]
        EApp (EVar (Name "source" _)) [ELit (LitString _)] ->
            [Warning Warn (path <> ".expr") "expression is source text reference (not translated)"]
        _ -> []

-- * Expressions

validateExpr :: Text -> Expr -> [Warning]
validateExpr path = \case
    EApp f args ->
        [Warning Info path "EApp with empty args list" | null args]
            <> validateExpr (path <> ".fun") f
            <> concatMap (\(i, a) -> validateExpr (argPath i) a) (zip [(0 :: Int) ..] args)
      where
        argPath i = path <> ".args[" <> T.pack (show i) <> "]"
    ECase scrut branches ->
        [Warning Warn path "ECase with empty branches" | null branches]
            <> validateExpr (path <> ".scrutinee") scrut
            <> concatMap (\(i, b) -> validateExpr (brPath i) (brBody b)) (zip [(0 :: Int) ..] branches)
      where
        brPath i = path <> ".branches[" <> T.pack (show i) <> "]"
    ELet binds body ->
        [Warning Warn path "ELet with empty binds" | null binds]
            <> concatMap (\(i, lb) -> validateExpr (bindPath i) (lbExpr lb)) (zip [(0 :: Int) ..] binds)
            <> validateExpr (path <> ".body") body
      where
        bindPath i = path <> ".binds[" <> T.pack (show i) <> "]"
    ELam params body ->
        [Warning Warn path "ELam with empty params" | null params]
            <> validateExpr (path <> ".body") body
    ECon _ args -> concatMap (\(i, a) -> validateExpr (path <> ".args[" <> T.pack (show i) <> "]") a) (zip [(0 :: Int) ..] args)
    ETypeApp e _ -> validateExpr (path <> ".expr") e
    ETypeLam _ e -> validateExpr (path <> ".body") e
    EPerform _ _ args -> concatMap (\(i, a) -> validateExpr (path <> ".args[" <> T.pack (show i) <> "]") a) (zip [(0 :: Int) ..] args)
    EHandle _ handler body -> validateExpr (path <> ".handler") handler <> validateExpr (path <> ".body") body
    EDelay e -> validateExpr (path <> ".inner") e
    EForce e -> validateExpr (path <> ".inner") e
    ETuple es -> concatMap (\(i, e) -> validateExpr (path <> ".elems[" <> T.pack (show i) <> "]") e) (zip [(0 :: Int) ..] es)
    EList es -> concatMap (\(i, e) -> validateExpr (path <> ".elems[" <> T.pack (show i) <> "]") e) (zip [(0 :: Int) ..] es)
    ERaise e -> validateExpr (path <> ".inner") e
    -- Leaf nodes: EVar, ELit, ERetain, ERelease, EDrop, EReuse, EUnreachable
    _ -> []

-- * Types

validateTy :: Text -> Ty -> [Warning]
validateTy path = \case
    TFn args _eff ret ->
        [Warning Warn path "TFn with empty args list" | null args]
            <> concatMap (\(i, a) -> validateTy (path <> ".args[" <> T.pack (show i) <> "]") (fnArgType a)) (zip [(0 :: Int) ..] args)
            <> validateTy (path <> ".ret") ret
    TForall _ ty -> validateTy (path <> ".body") ty
    TApp _ tys -> concatMap (\(i, t) -> validateTy (path <> ".args[" <> T.pack (show i) <> "]") t) (zip [(0 :: Int) ..] tys)
    TSyn _ ty -> validateTy (path <> ".expanded") ty
    -- Leaf nodes: TCon, TVar, TAny
    _ -> []

-- * Data types

validateDataType :: Text -> DataType -> [Warning]
validateDataType path dt =
    [Warning Warn path "data type has no constructors" | null (dtConstructors dt)]
        <> concatMap
            (\(i, c) -> validateConstructor (path <> ".constructors[" <> T.pack (show i) <> "]") c)
            (zip [(0 :: Int) ..] (dtConstructors dt))

validateConstructor :: Text -> Constructor -> [Warning]
validateConstructor path con
    | T.null (nameText (qnName (conName con))) =
        [Warning Error (path <> ".name") "constructor name is empty"]
    | otherwise = []
