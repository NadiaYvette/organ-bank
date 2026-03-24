-- | Efficient Text-based JSON emission for OrganIR.
module OrganIR.Json (renderOrganIR) where

import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Types

-- | Render a complete OrganIR document to JSON text.
renderOrganIR :: OrganIR -> Text
renderOrganIR (OrganIR meta modul) =
    obj
        [ ("schema_version", str schemaVersion)
        , ("metadata", renderMetadata meta)
        , ("module", renderModule modul)
        ]

renderMetadata :: Metadata -> Text
renderMetadata m =
    obj $
        [("source_language", str (sourceLangText (metaSourceLang m)))]
            ++ opt "compiler_version" str (metaCompilerVersion m)
            ++ opt "source_file" str (metaSourceFile m)
            ++ [("shim_version", str (metaShimVersion m))]
            ++ opt "timestamp" str (metaTimestamp m)

renderModule :: Module -> Text
renderModule m =
    obj $
        ("name", str (modName m))
            : [("exports", arr (map str (modExports m))) | not (null (modExports m))]
            ++ [("definitions", arr (map renderDef (modDefs m)))]
            ++ [("data_types", arr (map renderDataType (modDataTypes m))) | not (null (modDataTypes m))]
            ++ [("effect_decls", arr (map renderEffectDecl (modEffectDecls m))) | not (null (modEffectDecls m))]

renderDef :: Definition -> Text
renderDef d =
    obj
        [ ("name", renderQName (defName d))
        , ("type", renderTy (defType d))
        , ("expr", renderExpr (defExpr d))
        , ("sort", str (sortText (defSort d)))
        , ("visibility", str (visText (defVisibility d)))
        , ("arity", int (fromIntegral (defArity d)))
        ]

renderQName :: QName -> Text
renderQName (QName m n) = obj [("module", str m), ("name", renderName n)]

renderName :: Name -> Text
renderName (Name t u) = obj [("text", str t), ("unique", int (fromIntegral u))]

renderTy :: Ty -> Text
renderTy = \case
    TForall vars body ->
        obj [("forall", obj [("vars", arr (map renderTyVar vars)), ("body", renderTy body)])]
    TFn args eff result ->
        obj [("fn", obj [("args", arr (map renderFnArg args)), ("effect", renderEffectRow eff), ("result", renderTy result)])]
    TApp con args ->
        obj [("app", obj [("con", renderQName con), ("args", arr (map renderTy args))])]
    TCon qn ->
        obj [("con", obj [("qname", renderQName qn)])]
    TVar n ->
        obj [("var", renderName n)]
    TSyn n expansion ->
        obj [("syn", obj [("name", renderQName n), ("expansion", renderTy expansion)])]
    TAny ->
        obj [("con", obj [("qname", renderQName (QName "" (Name "any" 0)))])]

renderFnArg :: FnArg -> Text
renderFnArg (FnArg mult ty) =
    obj $
        opt "multiplicity" (str . mulText) mult
            ++ [("type", renderTy ty)]

renderTyVar :: TyVar -> Text
renderTyVar (TyVar n kind) =
    obj $ ("name", renderName n) : opt "kind" str kind

renderEffectRow :: EffectRow -> Text
renderEffectRow (EffectRow effs tail_) =
    obj $
        ("effects", arr (map renderQName effs))
            : opt "tail" renderName tail_

renderExpr :: Expr -> Text
renderExpr = \case
    EVar n -> obj [("evar", renderName n)]
    ELit lit -> obj [("elit", renderLit lit)]
    ECon qn args ->
        obj [("econ", obj $ ("name", renderQName qn) : optArr "args" renderExpr args)]
    EApp fn args ->
        obj [("eapp", obj [("fn", renderExpr fn), ("args", arr (map renderExpr args))])]
    ELam params body ->
        obj [("elam", obj [("params", arr (map renderLamParam params)), ("body", renderExpr body)])]
    ELet binds body ->
        obj [("elet", obj [("binds", arr (map renderLetBind binds)), ("body", renderExpr body)])]
    ECase scrut branches ->
        obj [("ecase", obj [("scrutinee", renderExpr scrut), ("branches", arr (map renderBranch branches))])]
    ETypeApp e tys ->
        obj [("etype_app", obj [("expr", renderExpr e), ("types", arr (map renderTy tys))])]
    ETypeLam vars body ->
        obj [("etype_lam", obj [("vars", arr (map renderTyVar vars)), ("body", renderExpr body)])]
    EPerform eff op args ->
        obj [("eperform", obj $ [("effect", renderQName eff), ("op", str op)] ++ optArr "args" renderExpr args)]
    EHandle eff body handler ->
        obj [("ehandle", obj [("effect", renderQName eff), ("body", renderExpr body), ("handler", renderExpr handler)])]
    ERetain n -> obj [("eretain", renderName n)]
    ERelease n -> obj [("erelease", renderName n)]
    EDrop n -> obj [("edrop", renderName n)]
    EReuse n -> obj [("ereuse", renderName n)]
    EDelay e -> obj [("edelay", renderExpr e)]
    EForce e -> obj [("eforce", renderExpr e)]
    ETuple es -> obj [("econ", obj [("name", renderQName (QName "" (Name "tuple" 0))), ("args", arr (map renderExpr es))])]
    EList es -> obj [("econ", obj [("name", renderQName (QName "" (Name "list" 0))), ("args", arr (map renderExpr es))])]
    ERaise e -> obj [("eperform", obj [("effect", renderQName (QName "" (Name "exn" 0))), ("op", str "raise"), ("args", arr [renderExpr e])])]
    EUnreachable -> obj [("evar", renderName (Name "_unreachable" 0))]

renderLit :: Lit -> Text
renderLit = \case
    LitInt n -> obj [("int", int n)]
    LitFloat d -> obj [("float", T.pack (show d))]
    LitString s -> obj [("string", str s)]
    LitBool b -> obj [("bool", if b then "true" else "false")]

renderLamParam :: LamParam -> Text
renderLamParam (LamParam n mty) =
    obj $ ("name", renderName n) : opt "type" renderTy mty

renderLetBind :: LetBind -> Text
renderLetBind (LetBind n mty e) =
    obj $ ("name", renderName n) : opt "type" renderTy mty ++ [("expr", renderExpr e)]

renderBranch :: Branch -> Text
renderBranch (Branch pat body) =
    obj [("pattern", renderPat pat), ("body", renderExpr body)]

renderPat :: Pat -> Text
renderPat = \case
    PatCon qn binders ->
        obj [("pat_con", obj $ ("name", renderQName qn) : optArr "args" renderPatBinder binders)]
    PatLit lit ->
        obj [("pat_lit", renderLit lit)]
    PatVar n mty ->
        obj [("pat_var", obj $ ("name", renderName n) : opt "type" renderTy mty)]
    PatWild ->
        obj [("pat_wild", obj [])]

renderPatBinder :: PatBinder -> Text
renderPatBinder (PatBinder n mty) =
    obj $ ("name", renderName n) : opt "type" renderTy mty

renderDataType :: DataType -> Text
renderDataType (DataType qn tparams ctors) =
    obj $
        [("name", renderQName qn)]
            ++ optArr "type_params" renderTyVar tparams
            ++ [("constructors", arr (map renderCtor ctors))]

renderCtor :: Constructor -> Text
renderCtor (Constructor qn fields) =
    obj [("name", renderQName qn), ("fields", arr (map renderTy fields))]

renderEffectDecl :: EffectDecl -> Text
renderEffectDecl (EffectDecl qn tparams ops) =
    obj $
        [("name", renderQName qn)]
            ++ optArr "type_params" renderTyVar tparams
            ++ [("operations", arr (map renderOp ops))]

renderOp :: Operation -> Text
renderOp (Operation n ty) =
    obj [("name", str n), ("type", renderTy ty)]

-- * JSON primitives

obj :: [(Text, Text)] -> Text
obj [] = "{}"
obj fields = T.concat ["{", T.intercalate ", " (map (\(k, v) -> T.concat [str k, ": ", v]) fields), "}"]

arr :: [Text] -> Text
arr [] = "[]"
arr items = T.concat ["[", T.intercalate ", " items, "]"]

str :: Text -> Text
str t = T.concat ["\"", T.concatMap esc t, "\""]
  where
    esc '"' = "\\\""; esc '\\' = "\\\\"; esc '\n' = "\\n"; esc '\t' = "\\t"; esc c = T.singleton c

int :: Integer -> Text
int = T.pack . show

opt :: Text -> (a -> Text) -> Maybe a -> [(Text, Text)]
opt _ _ Nothing = []
opt k f (Just v) = [(k, f v)]

optArr :: Text -> (a -> Text) -> [a] -> [(Text, Text)]
optArr _ _ [] = []
optArr k f xs = [(k, arr (map f xs))]

-- * Enum text rendering

sourceLangText :: SourceLang -> Text
sourceLangText = \case
    LHaskell -> "haskell"
    LRust -> "rust"
    LMercury -> "mercury"
    LIdris2 -> "idris2"
    LLean4 -> "lean4"
    LKoka -> "koka"
    LOCaml -> "ocaml"
    LSwift -> "swift"
    LErlang -> "erlang"
    LPurescript -> "purescript"
    LAgda -> "agda"
    LFSharp -> "fsharp"
    LScala3 -> "scala3"
    LJulia -> "julia"
    LZig -> "zig"
    LC -> "c"
    LCpp -> "cpp"
    LFortran -> "fortran"
    LAda -> "ada"
    LSml -> "sml"
    LCommonLisp -> "common-lisp"
    LScheme -> "scheme"
    LProlog -> "prolog"
    LLua -> "lua"
    LForth -> "forth"

sortText :: Sort -> Text
sortText = \case SFun -> "fun"; SVal -> "val"; SExternal -> "external"; SCon -> "con"

visText :: Visibility -> Text
visText = \case Public -> "public"; Private -> "private"

mulText :: Multiplicity -> Text
mulText = \case Many -> "many"; Affine -> "affine"; Linear -> "linear"
