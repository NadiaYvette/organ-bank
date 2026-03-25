{- | Translate Lua AST to OrganIR.
Strategy: top-level function definitions → Definition, other statements → main.
Local variables → let bindings, control flow → eIf/loops.
-}
module LuaFrontend.ToOrganIR (emitLuaIR) where

import Data.Text (Text)
import Data.Text qualified as T
import LuaFrontend.AST
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR

-- | Emit OrganIR JSON for a Lua program.
emitLuaIR :: String -> FilePath -> Block -> Text
emitLuaIR modName srcFile block =
    let defs = blockToDefs block
        exports = map (IR.nameText . IR.qnName . IR.defName) defs
    in  renderOrganIR $
            IR.organIRWithExports IR.LLua "lua-frontend-0.1" (T.pack modName) srcFile exports defs

blockToDefs :: Block -> [IR.Definition]
blockToDefs = concatMap statToDef

statToDef :: Stat -> [IR.Definition]
statToDef = \case
    SFunc (FuncName names meth) params vararg body ->
        let nm = T.intercalate "." names <> maybe "" (":" <>) meth
            paramNames = params ++ ["..." | vararg]
            bodyExpr = blockToIR body
            expr = IR.ELam (map (\p -> IR.LamParam (IR.name p) Nothing) paramNames) bodyExpr
         in [IR.funDef nm IR.TAny expr (length params)]
    SLocalFunc nm params vararg body ->
        let paramNames = params ++ ["..." | vararg]
            bodyExpr = blockToIR body
            expr = IR.ELam (map (\p -> IR.LamParam (IR.name p) Nothing) paramNames) bodyExpr
         in [IR.funDef nm IR.TAny expr (length params)]
    SLocal names exprs ->
        zipWith (\n e -> IR.valDefSimple n (exprToIR e)) names (padExprs names exprs)
    SAssign vars exprs ->
        let es = padExprs vars exprs
         in zipWith (\v e -> IR.valDefSimple (lvarName v) (exprToIR e)) vars es
    other -> [IR.valDefSimple "_stmt" (statToIR other)]

-- | Pad expression list with nils to match a target list length.
padExprs :: [a] -> [Expr] -> [Expr]
padExprs targets exprs =
    let n = length targets
     in take n (exprs ++ repeat ENil)

lvarName :: LVar -> Text
lvarName (LVName n) = n
lvarName (LVIndex _ _) = "_index"
lvarName (LVField _ f) = f

-- | Convert a block to an IR expression (sequence).
blockToIR :: Block -> IR.Expr
blockToIR [] = IR.eNil
blockToIR stmts = IR.eSeq (map statToIR stmts)

-- | Convert a statement to an IR expression.
statToIR :: Stat -> IR.Expr
statToIR = \case
    SAssign vars exprs ->
        let es = padExprs vars exprs
         in IR.eSeq (zipWith (\v e -> IR.EApp (IR.eVar "assign") [lvarToIR v, exprToIR e]) vars es)
    SLocal names exprs ->
        let es = padExprs names exprs
         in IR.eLet (zip names (map exprToIR es)) IR.eNil
    SLocalFunc nm params vararg body ->
        IR.eLet1 nm (funcToIR params vararg body) IR.eNil
    SDo body -> blockToIR body
    SWhile cond body ->
        IR.EApp (IR.eVar "while") [IR.ELam [] (exprToIR cond), IR.ELam [] (blockToIR body)]
    SRepeat body cond ->
        IR.EApp (IR.eVar "repeat_until") [IR.ELam [] (blockToIR body), IR.ELam [] (exprToIR cond)]
    SIf cond body elseifs elsePart ->
        ifToIR cond body elseifs elsePart
    SForNum var start stop step body ->
        IR.EApp
            (IR.eVar "for_num")
            [ IR.eString var
            , exprToIR start
            , exprToIR stop
            , maybe IR.eNil exprToIR step
            , IR.ELam [IR.LamParam (IR.name var) Nothing] (blockToIR body)
            ]
    SForIn names exprs body ->
        IR.EApp
            (IR.eVar "for_in")
            [ IR.EList (map IR.eString names)
            , IR.EList (map exprToIR exprs)
            , IR.ELam (map (\n -> IR.LamParam (IR.name n) Nothing) names) (blockToIR body)
            ]
    SReturn exprs -> case exprs of
        [] -> IR.eNil
        [e] -> exprToIR e
        es -> IR.ETuple (map exprToIR es)
    SBreak -> IR.EApp (IR.eVar "break") []
    SFunc (FuncName names meth) params vararg body ->
        let nm = T.intercalate "." names <> maybe "" (":" <>) meth
         in IR.EApp (IR.eVar "assign") [IR.eVar nm, funcToIR params vararg body]
    SExprStat e -> exprToIR e

funcToIR :: [Text] -> Bool -> Block -> IR.Expr
funcToIR params vararg body =
    let paramNames = params ++ ["..." | vararg]
     in IR.ELam (map (\p -> IR.LamParam (IR.name p) Nothing) paramNames) (blockToIR body)

ifToIR :: Expr -> Block -> [(Expr, Block)] -> Maybe Block -> IR.Expr
ifToIR cond body [] Nothing =
    IR.eIf (exprToIR cond) (blockToIR body) IR.eNil
ifToIR cond body [] (Just elsePart) =
    IR.eIf (exprToIR cond) (blockToIR body) (blockToIR elsePart)
ifToIR cond body ((c2, b2) : rest) elsePart =
    IR.eIf (exprToIR cond) (blockToIR body) (ifToIR c2 b2 rest elsePart)

lvarToIR :: LVar -> IR.Expr
lvarToIR = \case
    LVName n -> IR.eVar n
    LVIndex e k -> IR.EApp (IR.eVar "index") [exprToIR e, exprToIR k]
    LVField e f -> IR.EApp (IR.eVar "field") [exprToIR e, IR.eString f]

-- | Translate a Lua expression to OrganIR.
exprToIR :: Expr -> IR.Expr
exprToIR = \case
    ENil -> IR.eNil
    ETrue -> IR.eBool True
    EFalse -> IR.eBool False
    EInt n -> IR.eInt n
    EFloat d -> IR.eFloat d
    EString s -> IR.eString s
    EVarArg -> IR.eVar "..."
    EName n -> IR.eVar n
    EIndex e k -> IR.EApp (IR.eVar "index") [exprToIR e, exprToIR k]
    EField e f -> IR.EApp (IR.eVar "field") [exprToIR e, IR.eString f]
    EMethodCall obj meth args ->
        IR.EApp (IR.EApp (IR.eVar ":") [exprToIR obj, IR.eString meth]) (map exprToIR args)
    ECall f args -> IR.EApp (exprToIR f) (map exprToIR args)
    EBinOp op l r -> IR.EApp (IR.eVar op) [exprToIR l, exprToIR r]
    EUnOp op e -> IR.EApp (IR.eVar op) [exprToIR e]
    EFunc params vararg body -> funcToIR params vararg body
    ETable fields -> IR.EApp (IR.eVar "table") (map fieldToIR fields)
  where
    fieldToIR (FKey k v) = IR.ETuple [exprToIR k, exprToIR v]
    fieldToIR (FName n v) = IR.ETuple [IR.eString n, exprToIR v]
    fieldToIR (FExpr e) = exprToIR e
