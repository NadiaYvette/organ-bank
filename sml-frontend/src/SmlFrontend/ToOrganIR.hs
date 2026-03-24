-- | Translate SML AST + inferred types to OrganIR JSON.
module SmlFrontend.ToOrganIR (emitOrganIR) where

import Data.List (unsnoc)
import Data.Text (Text)
import Data.Text qualified as T
import SmlFrontend.Elab.Env
import SmlFrontend.Elab.Infer (InferState (..))
import SmlFrontend.Elab.Types
import SmlFrontend.Syntax.AST
import SmlFrontend.Syntax.Const
import SmlFrontend.Syntax.Ident

-- | Emit OrganIR JSON for a program with inferred types.
emitOrganIR :: String -> FilePath -> Program -> InferState -> Text
emitOrganIR modName srcFile (Program decs) st =
    let defs = concatMap (decToDefs st) decs
     in T.concat
            [ "{\n  \"source_language\": \"sml\",\n"
            , "  \"module_name\": "
            , jsonStr (T.pack modName)
            , ",\n"
            , "  \"source_file\": "
            , jsonStr (T.pack srcFile)
            , ",\n"
            , "  \"compiler_version\": \"sml-frontend-0.1\",\n"
            , "  \"definitions\": [\n"
            , T.intercalate ",\n" defs
            , "\n  ]\n}\n"
            ]

decToDefs :: InferState -> Dec -> [Text]
decToDefs st (DVal _ binds) = concatMap (valBindToDef st) binds
decToDefs st (DFun _ binds) = concatMap (funBindToDef st) binds
decToDefs _ (DDatatype datbinds) = concatMap datbindToDef datbinds
decToDefs st (DLocal _ decs) = concatMap (decToDefs st) decs
decToDefs st (DSeq decs) = concatMap (decToDefs st) decs
decToDefs st (DPos _ d) = decToDefs st d
decToDefs _ _ = []

valBindToDef :: InferState -> ValBind -> [Text]
valBindToDef st (ValBind _ pat exp_) =
    case patName pat of
        Just name ->
            let ty = lookupType name st
             in [emitDef name ty (expToJson exp_) (expArity exp_)]
        Nothing -> []

funBindToDef :: InferState -> FunBind -> [Text]
funBindToDef st (FunBind clauses@(FunClause (VId name) pats _ _ : _)) =
    let ty = lookupType (T.unpack name) st
        arity = length pats
        bodyJson = case clauses of
            [FunClause _ ps _ body] ->
                let paramNames = concatMap patNames ps
                    params = T.concat ["[", T.intercalate ", " (map (\p -> T.concat ["[", jsonStr p, ", 0]"]) paramNames), "]"]
                 in T.concat ["{\"tag\": \"lam\", \"params\": ", params, ", \"body\": ", expToJson body, "}"]
            _ ->
                -- Multiple clauses: emit as case on args
                T.concat
                    [ "{\"tag\": \"lam\", \"params\": [[\"_arg\", 0]], \"body\": "
                    , "{\"tag\": \"case\", \"scrutinee_name\": \"_arg\", \"scrutinee_unique\": 0, \"branches\": ["
                    , T.intercalate ", " (map clauseToJson clauses)
                    , "]}}"
                    ]
     in [emitDef (T.unpack name) ty bodyJson arity]
funBindToDef _ (FunBind []) = []

clauseToJson :: FunClause -> Text
clauseToJson (FunClause _ pats _ body) =
    let _patJson = case pats of
            [p] -> patToJson p
            _ -> T.concat ["{\"tag\": \"tuple\", \"elements\": [", T.intercalate ", " (map patToJson pats), "]}"]
        binderNames = concatMap patNames pats
        binders = T.concat ["[", T.intercalate ", " (map (\n -> T.concat ["[", jsonStr n, ", 0]"]) binderNames), "]"]
     in T.concat ["{\"con\": \"_\", \"binders\": ", binders, ", \"body\": ", expToJson body, "}"]

lookupType :: String -> InferState -> Text
lookupType name st =
    case lookupEnv name (isEnv st) of
        Just (Scheme _ ty) -> tyToJson (apply (isSubst st) ty)
        Nothing -> "{\"tag\": \"any\"}"

tyToJson :: ITy -> Text
tyToJson (ITyVar _) = "{\"tag\": \"any\"}"
tyToJson (ITyCon name []) = T.concat ["{\"tag\": \"con\", \"name\": ", jsonStr (T.pack name), "}"]
tyToJson (ITyCon name args) =
    T.concat
        [ "{\"tag\": \"app\", \"con\": "
        , jsonStr (T.pack name)
        , ", \"args\": ["
        , T.intercalate ", " (map tyToJson args)
        , "]}"
        ]
tyToJson (ITyFun a b) =
    T.concat
        ["{\"tag\": \"fn\", \"params\": [", tyToJson a, "], \"result\": ", tyToJson b, "}"]
tyToJson (ITyTuple ts) =
    T.concat
        ["{\"tag\": \"con\", \"name\": \"tuple\", \"args\": [", T.intercalate ", " (map tyToJson ts), "]}"]
tyToJson (ITyRecord _fs) =
    T.concat
        ["{\"tag\": \"con\", \"name\": \"record\"}"]

expToJson :: Exp -> Text
expToJson (ESCon (SInt n)) = T.concat ["{\"tag\": \"lit_int\", \"value\": ", T.pack (show n), "}"]
expToJson (ESCon (SReal d)) = T.concat ["{\"tag\": \"lit_float\", \"value\": ", T.pack (show d), "}"]
expToJson (ESCon (SString s)) = T.concat ["{\"tag\": \"lit_str\", \"value\": ", jsonStr s, "}"]
expToJson (ESCon (SChar c)) = T.concat ["{\"tag\": \"lit_str\", \"value\": ", jsonStr (T.singleton c), "}"]
expToJson (EVar (LongVId _ (VId name))) = T.concat ["{\"tag\": \"var\", \"name\": ", jsonStr name, ", \"unique\": 0}"]
expToJson (EApp f arg) =
    let fJson = expToJson f
        argJson = expToJson arg
     in T.concat ["{\"tag\": \"app\", \"fn\": ", fJson, ", \"arg\": ", argJson, "}"]
expToJson (EInfix l (VId op) r) =
    T.concat
        [ "{\"tag\": \"app\", \"fn\": {\"tag\": \"var\", \"name\": "
        , jsonStr op
        , ", \"unique\": 0}, \"arg\": {\"tag\": \"tuple\", \"elements\": ["
        , expToJson l
        , ", "
        , expToJson r
        , "]}}"
        ]
expToJson (EFn [MRule pat body]) =
    let names = patNames pat
        params = T.concat ["[", T.intercalate ", " (map (\n -> T.concat ["[", jsonStr n, ", 0]"]) names), "]"]
     in T.concat ["{\"tag\": \"lam\", \"params\": ", params, ", \"body\": ", expToJson body, "}"]
expToJson (EFn _) = "{\"tag\": \"lam\", \"params\": [], \"body\": {\"tag\": \"unreachable\"}}"
expToJson (ELet decs body) =
    T.concat ["{\"tag\": \"let_block\", \"decs\": ", T.pack (show (length decs)), ", \"body\": ", expToJson body, "}"]
expToJson (EIf _c t e) =
    T.concat
        [ "{\"tag\": \"case\", \"scrutinee_name\": \"_if\", \"scrutinee_unique\": 0, \"branches\": ["
        , "{\"con\": \"true\", \"binders\": [], \"body\": "
        , expToJson t
        , "}, "
        , "{\"con\": \"false\", \"binders\": [], \"body\": "
        , expToJson e
        , "}"
        , "]}"
        ]
expToJson (ECase scrut rules) =
    T.concat
        [ "{\"tag\": \"case\", \"scrutinee\": "
        , expToJson scrut
        , ", \"branches\": ["
        , T.intercalate ", " (map mruleToJson rules)
        , "]}"
        ]
expToJson (ETuple es) = T.concat ["{\"tag\": \"tuple\", \"elements\": [", T.intercalate ", " (map expToJson es), "]}"]
expToJson (EList es) = T.concat ["{\"tag\": \"list\", \"elements\": [", T.intercalate ", " (map expToJson es), "]}"]
expToJson (ERaise e) = T.concat ["{\"tag\": \"raise\", \"exn\": ", expToJson e, "}"]
expToJson (ESeq []) = "{\"tag\": \"tuple\", \"elements\": []}"
expToJson (ESeq es) = case unsnoc es of
    Just (_, e) -> expToJson e
    Nothing -> "{\"tag\": \"tuple\", \"elements\": []}"
expToJson (EAndalso a b) = expToJson (EIf a b (EVar (LongVId [] (VId "false"))))
expToJson (EOrelse a b) = expToJson (EIf a (EVar (LongVId [] (VId "true"))) b)
expToJson (ETyped e _) = expToJson e
expToJson (EPos _ e) = expToJson e
expToJson _ = "{\"tag\": \"unreachable\"}"

mruleToJson :: MRule -> Text
mruleToJson (MRule pat body) =
    let binders = patNames pat
        bs = T.concat ["[", T.intercalate ", " (map (\n -> T.concat ["[", jsonStr n, ", 0]"]) binders), "]"]
     in T.concat ["{\"con\": \"_\", \"binders\": ", bs, ", \"body\": ", expToJson body, "}"]

patToJson :: Pat -> Text
patToJson (PVar (VId n)) = T.concat ["{\"tag\": \"var\", \"name\": ", jsonStr n, "}"]
patToJson PWild = "{\"tag\": \"wild\"}"
patToJson (PSCon (SInt n)) = T.concat ["{\"tag\": \"lit\", \"value\": ", T.pack (show n), "}"]
patToJson (PCon (LongVId _ (VId n)) _) = T.concat ["{\"tag\": \"con\", \"name\": ", jsonStr n, "}"]
patToJson _ = "{\"tag\": \"wild\"}"

-- | Extract the name from a simple pattern.
patName :: Pat -> Maybe String
patName (PVar (VId n)) = Just (T.unpack n)
patName (PPos _ p) = patName p
patName (PTyped p _) = patName p
patName _ = Nothing

patNames :: Pat -> [Text]
patNames (PVar (VId n)) = [n]
patNames (PTuple ps) = concatMap patNames ps
patNames (PAs (VId n) p) = n : patNames p
patNames (PPos _ p) = patNames p
patNames (PTyped p _) = patNames p
patNames _ = ["_"]

expArity :: Exp -> Int
expArity (EFn _) = 1
expArity _ = 0

emitDef :: String -> Text -> Text -> Int -> Text
emitDef name ty bodyJson arity =
    T.concat
        [ "    {\n"
        , "      \"name\": {\"module\": \"\", \"text\": "
        , jsonStr (T.pack name)
        , ", \"unique\": 0},\n"
        , "      \"type\": "
        , ty
        , ",\n"
        , "      \"expr\": "
        , bodyJson
        , ",\n"
        , "      \"sort\": \"fun\",\n"
        , "      \"visibility\": \"public\",\n"
        , "      \"arity\": "
        , T.pack (show arity)
        , "\n"
        , "    }"
        ]

datbindToDef :: DatBind -> [Text]
datbindToDef (DatBind _ (TyCon tcName) conbinds) =
    map (conbindToDef tcName) conbinds

conbindToDef :: Text -> ConBind -> Text
conbindToDef tcName (ConBind (VId cname) argTy) =
    let arity = case argTy of Nothing -> (0 :: Int); Just _ -> 1
     in T.concat
            [ "    {\n"
            , "      \"name\": {\"module\": \"\", \"text\": "
            , jsonStr cname
            , ", \"unique\": 0},\n"
            , "      \"type\": {\"tag\": \"con\", \"name\": "
            , jsonStr tcName
            , "},\n"
            , "      \"expr\": {\"tag\": \"con\", \"name\": "
            , jsonStr cname
            , "},\n"
            , "      \"sort\": \"con\",\n"
            , "      \"visibility\": \"public\",\n"
            , "      \"arity\": "
            , T.pack (show arity)
            , "\n"
            , "    }"
            ]

jsonStr :: Text -> Text
jsonStr t = T.concat ["\"", T.concatMap esc t, "\""]
  where
    esc '"' = "\\\""; esc '\\' = "\\\\"; esc '\n' = "\\n"; esc '\t' = "\\t"; esc c = T.singleton c
