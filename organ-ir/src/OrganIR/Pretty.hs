-- | Human-readable pretty-printer for OrganIR documents.
module OrganIR.Pretty (ppOrganIR, ppDefinition, ppExpr, ppTy, ppDataType, ppEffectDecl) where

import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Types

-- | Pretty-print a full OrganIR document with metadata header.
ppOrganIR :: OrganIR -> Text
ppOrganIR ir =
    let meta = irMetadata ir
        m = irModule ir
        header =
            T.unlines $
                ( "-- module: "
                    <> modName m
                    <> " ("
                    <> ppSourceLang (metaSourceLang meta)
                    <> ", "
                    <> metaShimVersion meta
                    <> ")"
                )
                    : maybe [] (\sf -> ["-- source: " <> sf]) (metaSourceFile meta)
        defs = T.intercalate "\n\n" (map ppDefinition (modDefs m))
        dts = T.intercalate "\n\n" (map ppDataType (modDataTypes m))
        effs = T.intercalate "\n\n" (map ppEffectDecl (modEffectDecls m))
        sections = filter (not . T.null) [defs, dts, effs]
     in header <> "\n" <> T.intercalate "\n\n" sections <> "\n"

-- | Pretty-print a source language tag.
ppSourceLang :: SourceLang -> Text
ppSourceLang = \case
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

-- | Pretty-print a single definition.
ppDefinition :: Definition -> Text
ppDefinition def =
    let kw = case defSort def of
            SFun -> "fun"
            SVal -> "val"
            SExternal -> "external"
            SCon -> "con"
        qn = ppQName (defName def)
        arity = defArity def
        arityStr = if arity > 0 || defSort def == SFun then "/" <> T.pack (show arity) else ""
        vis = case defVisibility def of
            Public -> ""
            Private -> "private "
        ty = ppTy (defType def)
        body = ppExprIndent 4 (defExpr def)
     in vis <> kw <> " " <> qn <> arityStr <> " : " <> ty <> "\n  = " <> body

-- | Pretty-print a type.
ppTy :: Ty -> Text
ppTy = \case
    TAny -> "any"
    TCon qn -> ppQName qn
    TApp qn tys -> ppQName qn <> "<" <> T.intercalate ", " (map ppTy tys) <> ">"
    TFn args eff ret ->
        let argsTxt = "(" <> T.intercalate ", " (map ppFnArg args) <> ")"
            arrow
                | null (erEffects eff) && case erTail eff of Nothing -> True; _ -> False = " -> "
                | otherwise = " -" <> ppEffectRow eff <> "-> "
         in argsTxt <> arrow <> ppTy ret
    TForall tvs ty -> "forall " <> T.unwords (map ppTyVar tvs) <> ". " <> ppTy ty
    TVar n -> ppName n
    TSyn qn _ty -> ppQName qn

-- | Pretty-print a function argument.
ppFnArg :: FnArg -> Text
ppFnArg fa =
    let mulTxt = case fnArgMultiplicity fa of
            Nothing -> ""
            Just Many -> ""
            Just Affine -> "affine "
            Just Linear -> "linear "
     in mulTxt <> ppTy (fnArgType fa)

-- | Pretty-print an effect row.
ppEffectRow :: EffectRow -> Text
ppEffectRow er =
    let effs = map ppQName (erEffects er)
        tail_ = case erTail er of
            Nothing -> []
            Just n -> [ppName n]
        all_ = effs ++ tail_
     in T.intercalate ", " all_

-- | Pretty-print a type variable.
ppTyVar :: TyVar -> Text
ppTyVar tv = ppName (tvName tv)

-- | Pretty-print an expression at a given indentation level.
ppExprIndent :: Int -> Expr -> Text
ppExprIndent ind = \case
    EVar n -> ppName n
    ELit lit -> ppLit lit
    ECon qn [] -> ppQName qn
    ECon qn args -> ppQName qn <> "(" <> T.intercalate ", " (map (ppExprIndent ind) args) <> ")"
    EApp f args -> ppExprIndent ind f <> "(" <> T.intercalate ", " (map (ppExprIndent ind) args) <> ")"
    ELam params body ->
        "\\("
            <> T.intercalate ", " (map ppLamParam params)
            <> ") -> "
            <> ppExprIndent ind body
    ELet binds body ->
        let pad = T.replicate ind " "
            ppBind (i :: Int, b) =
                let prefix = if i == 0 then "let " else pad <> "    "
                 in prefix <> ppName (lbName b) <> " = " <> ppExprIndent (ind + 4) (lbExpr b)
            bindLines = zipWith (curry ppBind) [0 ..] binds
         in T.intercalate "\n" bindLines <> "\n" <> pad <> "in " <> ppExprIndent (ind + 3) body
    ECase scrut branches ->
        let pad = T.replicate ind " "
            ppBranch br =
                pad
                    <> "  "
                    <> ppPat (brPattern br)
                    <> " -> "
                    <> ppExprIndent (ind + 5 + T.length (ppPat (brPattern br))) (brBody br)
         in "case "
                <> ppExprIndent ind scrut
                <> " of\n"
                <> T.intercalate "\n" (map ppBranch branches)
    ETuple es -> "(" <> T.intercalate ", " (map (ppExprIndent ind) es) <> ")"
    EList es -> "[" <> T.intercalate ", " (map (ppExprIndent ind) es) <> "]"
    ERaise e -> "raise " <> ppExprIndent ind e
    EUnreachable -> "unreachable"
    EDelay e -> "delay(" <> ppExprIndent ind e <> ")"
    EForce e -> "force(" <> ppExprIndent ind e <> ")"
    ERetain n -> "retain(" <> ppName n <> ")"
    ERelease n -> "release(" <> ppName n <> ")"
    EDrop n -> "drop(" <> ppName n <> ")"
    EReuse n -> "reuse(" <> ppName n <> ")"
    EPerform qn op args ->
        "perform " <> ppQName qn <> "." <> op <> "(" <> T.intercalate ", " (map (ppExprIndent ind) args) <> ")"
    EHandle qn handler body ->
        "handle " <> ppQName qn <> " " <> ppExprIndent ind handler <> " " <> ppExprIndent ind body
    ETypeApp e tys ->
        ppExprIndent ind e <> " @[" <> T.intercalate ", " (map ppTy tys) <> "]"
    ETypeLam tvs body ->
        "/\\(" <> T.intercalate ", " (map ppTyVar tvs) <> ") -> " <> ppExprIndent ind body

-- | Pretty-print an expression (top-level, no indentation).
ppExpr :: Expr -> Text
ppExpr = ppExprIndent 0

-- | Pretty-print a literal.
ppLit :: Lit -> Text
ppLit = \case
    LitInt n -> T.pack (show n)
    LitFloat d -> T.pack (show d)
    LitString s -> "\"" <> s <> "\""
    LitBool True -> "true"
    LitBool False -> "false"

-- | Pretty-print a pattern.
ppPat :: Pat -> Text
ppPat = \case
    PatCon qn [] -> ppQName qn
    PatCon qn binders ->
        ppQName qn <> "(" <> T.intercalate ", " (map (ppName . pbName) binders) <> ")"
    PatLit lit -> ppLit lit
    PatVar n _ty -> ppName n
    PatWild -> "_"

-- | Pretty-print a lambda parameter.
ppLamParam :: LamParam -> Text
ppLamParam lp = ppName (lpName lp)

-- | Pretty-print a qualified name.
ppQName :: QName -> Text
ppQName qn
    | T.null (qnModule qn) = ppName (qnName qn)
    | otherwise = qnModule qn <> "." <> ppName (qnName qn)

-- | Pretty-print a data type declaration.
ppDataType :: DataType -> Text
ppDataType dt =
    let params = case dtTypeParams dt of
            [] -> ""
            tvs -> " " <> T.unwords (map ppTyVar tvs)
        constrs = map ppConstructor (dtConstructors dt)
     in "data " <> ppQName (dtName dt) <> params <> "\n  = " <> T.intercalate "\n  | " constrs

-- | Pretty-print a constructor.
ppConstructor :: Constructor -> Text
ppConstructor con = case conFields con of
    [] -> ppQName (conName con)
    fs -> ppQName (conName con) <> "(" <> T.intercalate ", " (map ppTy fs) <> ")"

-- | Pretty-print an effect declaration.
ppEffectDecl :: EffectDecl -> Text
ppEffectDecl ed =
    let params = case edTypeParams ed of
            [] -> ""
            tvs -> " " <> T.unwords (map ppTyVar tvs)
        ops = map ppOperation (edOperations ed)
     in "effect " <> ppQName (edName ed) <> params <> "\n  " <> T.intercalate "\n  " ops

-- | Pretty-print an effect operation.
ppOperation :: Operation -> Text
ppOperation op = opName op <> " : " <> ppTy (opType op)

-- | Pretty-print a name, appending @#unique@ if unique > 0.
ppName :: Name -> Text
ppName n
    | nameUnique n > 0 = nameText n <> "#" <> T.pack (show (nameUnique n))
    | otherwise = nameText n
