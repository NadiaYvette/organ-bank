-- | Translate core Scheme AST to OrganIR using the organ-ir library.
module SchemeFrontend.ToOrganIR (emitSchemeIR) where

import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build
import OrganIR.Json (renderOrganIR)
import OrganIR.Types
import SchemeFrontend.AST

-- | Emit OrganIR JSON for a Scheme program.
emitSchemeIR :: String -> FilePath -> [TopLevel] -> Text
emitSchemeIR modName srcFile tops =
    let defs = concatMap topLevelToDefs tops
        exports = map (nameText . qnName . defName) defs
    in  renderOrganIR $
            OrganIR
                { irMetadata =
                    Metadata
                        { metaSourceLang = LScheme
                        , metaCompilerVersion = Nothing
                        , metaSourceFile = Just (T.pack srcFile)
                        , metaShimVersion = "scheme-frontend-0.1"
                        , metaTimestamp = Nothing
                        }
                , irModule =
                    Module
                        { modName = T.pack modName
                        , modExports = exports
                        , modImports = []
                        , modDefs = defs
                        , modDataTypes = []
                        , modEffectDecls = []
                        }
                }

topLevelToDefs :: TopLevel -> [Definition]
topLevelToDefs = \case
    TLDefine n body ->
        [funDef n TAny (coreToExpr body) (coreArity body)]
    TLDefineRec binds ->
        map (\(n, body) -> funDef n TAny (coreToExpr body) (coreArity body)) binds
    TLExpr body ->
        [funDef "_top" TAny (coreToExpr body) 0]

coreToExpr :: Core -> Expr
coreToExpr = \case
    CVar v -> EVar (name v)
    CLitInt n -> ELit (LitInt n)
    CLitFloat d -> ELit (LitFloat d)
    CLitString s -> ELit (LitString s)
    CLitChar c -> ELit (LitString (T.singleton c))
    CLitBool b -> ELit (LitBool b)
    CLam params body -> ELam (map (\p -> LamParam (name p) Nothing) params) (coreToExpr body)
    CApp fn args -> EApp (coreToExpr fn) (map coreToExpr args)
    CIf c t e ->
        ECase
            (coreToExpr c)
            [ Branch (PatCon (localName "true") []) (coreToExpr t)
            , Branch (PatCon (localName "false") []) (coreToExpr e)
            ]
    CLet binds body -> ELet (map letBind binds) (coreToExpr body)
    CLetRec binds body -> ELet (map letBind binds) (coreToExpr body)
    CSet v e -> EApp (EVar (name "set!")) [EVar (name v), coreToExpr e]
    CBegin [] -> ELit (LitBool False)
    CBegin [x] -> coreToExpr x
    CBegin xs -> ELet (zipWith seqBind [0 :: Int ..] (NE.init ne)) (coreToExpr (NE.last ne))
      where
        ne = NE.fromList xs -- safe: CBegin [] and CBegin [x] handled above
    CQuote q -> quoteToExpr q
    CVoid -> ELit (LitBool False)
  where
    letBind (n, e) = LetBind (name n) Nothing (coreToExpr e)
    seqBind i e = LetBind (name (T.pack ("_seq" ++ show i))) Nothing (coreToExpr e)

quoteToExpr :: QuoteVal -> Expr
quoteToExpr = \case
    QSymbol s -> ECon (localName "symbol") [ELit (LitString s)]
    QInt n -> ELit (LitInt n)
    QFloat d -> ELit (LitFloat d)
    QString s -> ELit (LitString s)
    QChar c -> ELit (LitString (T.singleton c))
    QBool b -> ELit (LitBool b)
    QList qs -> ECon (localName "list") (map quoteToExpr qs)
    QDottedList qs q ->
        foldr (\a b -> ECon (localName "cons") [quoteToExpr a, b]) (quoteToExpr q) qs
    QVector qs -> ECon (localName "vector") (map quoteToExpr qs)

coreArity :: Core -> Int
coreArity = \case CLam ps _ -> length ps; _ -> 0
