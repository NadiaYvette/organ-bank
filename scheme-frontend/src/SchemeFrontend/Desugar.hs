-- | Desugar datum tree → core AST. Handles R7RS-small derived forms.
module SchemeFrontend.Desugar (desugarProgram) where

import Control.Monad (mapAndUnzipM)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import SchemeFrontend.AST
import SchemeFrontend.Reader (Datum (..))

desugarProgram :: [Datum] -> Either String [TopLevel]
desugarProgram = mapM desugarTopLevel

desugarTopLevel :: Datum -> Either String TopLevel
desugarTopLevel = \case
    DList (DSymbol "define" : DSymbol name : body) ->
        TLDefine name <$> desugarBody body
    DList (DSymbol "define" : DList (DSymbol name : params) : body) -> do
        ps <- mapM expectSymbol params
        b <- desugarBody body
        pure $ TLDefine name (CLam ps b)
    DList [DSymbol "begin", DList (DSymbol "define" : _)] ->
        -- begin with internal defines → letrec
        Left "Top-level begin with defines not yet supported"
    d -> TLExpr <$> desugar d

desugar :: Datum -> Either String Core
desugar = \case
    DInt n -> Right (CLitInt n)
    DFloat d -> Right (CLitFloat d)
    DString s -> Right (CLitString s)
    DChar c -> Right (CLitChar c)
    DBool b -> Right (CLitBool b)
    DSymbol s -> Right (CVar s)
    DQuote d -> Right (CQuote (datumToQuote d))
    DQuasiquote d -> desugarQuasiquote d
    DList [] -> Right (CQuote (QList []))
    DList (DSymbol kw : args) -> desugarForm kw args
    DList (fn : args) -> do
        fn' <- desugar fn
        args' <- mapM desugar args
        Right (CApp fn' args')
    DVector ds -> do
        es <- mapM desugar ds
        Right (CApp (CVar "vector") es)
    DDottedList _ _ -> Left "Dotted list in expression context"
    DUnquote _ -> Left "Unquote outside quasiquote"
    DUnquoteSplicing _ -> Left "Unquote-splicing outside quasiquote"

desugarForm :: Text -> [Datum] -> Either String Core
desugarForm kw args = case kw of
    "lambda" -> case args of
        DList params : body -> do
            ps <- mapM expectSymbol params
            b <- desugarBody body
            Right (CLam ps b)
        _ -> Left "Bad lambda syntax"
    "if" -> case args of
        [c, t, e] -> CIf <$> desugar c <*> desugar t <*> desugar e
        [c, t] -> CIf <$> desugar c <*> desugar t <*> pure CVoid
        _ -> Left "Bad if syntax"
    "set!" -> case args of
        [DSymbol name, val] -> CSet name <$> desugar val
        _ -> Left "Bad set! syntax"
    "begin" -> CBegin <$> mapM desugar args
    "quote" -> case args of
        [d] -> Right (CQuote (datumToQuote d))
        _ -> Left "Bad quote syntax"
    "let" -> desugarLet args
    "let*" -> desugarLetStar args
    "letrec" -> desugarLetrec args
    "cond" -> desugarCond args
    "case" -> desugarCase args
    "and" -> desugarAnd args
    "or" -> desugarOr args
    "when" -> case args of
        c : body -> do
            c' <- desugar c
            b <- desugarBody body
            Right (CIf c' b CVoid)
        _ -> Left "Bad when syntax"
    "unless" -> case args of
        c : body -> do
            c' <- desugar c
            b <- desugarBody body
            Right (CIf c' CVoid b)
        _ -> Left "Bad unless syntax"
    "do" -> desugarDo args
    "define" -> Left "define in expression context"
    _ -> do
        args' <- mapM desugar args
        Right (CApp (CVar kw) args')

desugarLet :: [Datum] -> Either String Core
desugarLet = \case
    -- Named let: (let name ((var init) ...) body)
    DSymbol name : DList bindings : body -> do
        (vars, inits) <- unzipBindings bindings
        b <- desugarBody body
        Right (CLetRec [(name, CLam vars b)] (CApp (CVar name) inits))
    DList bindings : body -> do
        (vars, inits) <- unzipBindings bindings
        b <- desugarBody body
        Right (CLet (zip vars inits) b)
    _ -> Left "Bad let syntax"

desugarLetStar :: [Datum] -> Either String Core
desugarLetStar = \case
    DList bindings : body -> do
        bs <- mapM parseBinding bindings
        b <- desugarBody body
        Right (foldr (\(v, e) rest -> CLet [(v, e)] rest) b bs)
    _ -> Left "Bad let* syntax"

desugarLetrec :: [Datum] -> Either String Core
desugarLetrec = \case
    DList bindings : body -> do
        (vars, inits) <- unzipBindings bindings
        b <- desugarBody body
        Right (CLetRec (zip vars inits) b)
    _ -> Left "Bad letrec syntax"

desugarCond :: [Datum] -> Either String Core
desugarCond = \case
    [] -> Right CVoid
    [DList (DSymbol "else" : body)] -> desugarBody body
    DList (test : body) : rest -> do
        t <- desugar test
        b <- if null body then pure t else desugarBody body
        r <- desugarCond rest
        Right (CIf t b r)
    _ -> Left "Bad cond syntax"

desugarCase :: [Datum] -> Either String Core
desugarCase = \case
    key : clauses -> do
        k <- desugar key
        let tmp = "_case-key"
        branches <- desugarCaseClauses tmp clauses
        Right (CLet [(tmp, k)] branches)
    _ -> Left "Bad case syntax"

desugarCaseClauses :: Text -> [Datum] -> Either String Core
desugarCaseClauses _ [] = Right CVoid
desugarCaseClauses _ [DList (DSymbol "else" : body)] = desugarBody body
desugarCaseClauses tmp (DList (DList datums : body) : rest) = do
    tests <- mapM (\d -> Right (CApp (CVar "eqv?") [CVar tmp, CQuote (datumToQuote d)])) datums
    let test = foldl1 (\a b -> CIf a (CLitBool True) b) tests
    b <- desugarBody body
    r <- desugarCaseClauses tmp rest
    Right (CIf test b r)
desugarCaseClauses _ _ = Left "Bad case clause"

desugarAnd :: [Datum] -> Either String Core
desugarAnd = \case
    [] -> Right (CLitBool True)
    [x] -> desugar x
    x : xs -> do
        x' <- desugar x
        rest <- desugarAnd xs
        Right (CIf x' rest (CLitBool False))

desugarOr :: [Datum] -> Either String Core
desugarOr = \case
    [] -> Right (CLitBool False)
    [x] -> desugar x
    x : xs -> do
        x' <- desugar x
        rest <- desugarOr xs
        let tmp = "_or-tmp"
        Right (CLet [(tmp, x')] (CIf (CVar tmp) (CVar tmp) rest))

desugarDo :: [Datum] -> Either String Core
desugarDo = \case
    DList varSpecs : DList (test : exitBody) : commands -> do
        specs <- mapM parseDo varSpecs
        let (vars, inits, steps) = unzip3 specs
        t <- desugar test
        exit <- if null exitBody then pure CVoid else desugarBody exitBody
        body <- if null commands then pure CVoid else CBegin <$> mapM desugar commands
        let loopName = "_do-loop"
            stepExprs = zipWith (fromMaybe . CVar) vars steps
            loop =
                CLetRec
                    [(loopName, CLam vars (CIf t exit (CBegin [body, CApp (CVar loopName) stepExprs])))]
                    (CApp (CVar loopName) inits)
        Right loop
    _ -> Left "Bad do syntax"

parseDo :: Datum -> Either String (Text, Core, Maybe Core)
parseDo = \case
    DList [DSymbol v, init_] -> do
        i <- desugar init_
        Right (v, i, Nothing)
    DList [DSymbol v, init_, step] -> do
        i <- desugar init_
        s <- desugar step
        Right (v, i, Just s)
    _ -> Left "Bad do variable spec"

-- * Quasiquote

desugarQuasiquote :: Datum -> Either String Core
desugarQuasiquote = \case
    DUnquote d -> desugar d
    DList ds -> do
        parts <- mapM qqElement ds
        Right (foldr (\a b -> CApp (CVar "cons") [a, b]) (CQuote (QList [])) parts)
    d -> Right (CQuote (datumToQuote d))

qqElement :: Datum -> Either String Core
qqElement = \case
    DUnquote d -> desugar d
    DUnquoteSplicing _ -> Left "Splicing not yet supported in quasiquote"
    d -> desugarQuasiquote d

-- * Helpers

desugarBody :: [Datum] -> Either String Core
desugarBody = \case
    [] -> Left "Empty body"
    [x] -> desugar x
    xs -> CBegin <$> mapM desugar xs

expectSymbol :: Datum -> Either String Text
expectSymbol = \case
    DSymbol s -> Right s
    d -> Left $ "Expected symbol, got: " ++ show d

parseBinding :: Datum -> Either String (Text, Core)
parseBinding = \case
    DList [DSymbol v, e] -> (v,) <$> desugar e
    _ -> Left "Bad binding"

unzipBindings :: [Datum] -> Either String ([Text], [Core])
unzipBindings = mapAndUnzipM parseBinding

datumToQuote :: Datum -> QuoteVal
datumToQuote = \case
    DInt n -> QInt n
    DFloat d -> QFloat d
    DString s -> QString s
    DChar c -> QChar c
    DBool b -> QBool b
    DSymbol s -> QSymbol s
    DList ds -> QList (map datumToQuote ds)
    DDottedList ds d -> QDottedList (map datumToQuote ds) (datumToQuote d)
    DVector ds -> QVector (map datumToQuote ds)
    DQuote d -> QList [QSymbol "quote", datumToQuote d]
    DQuasiquote d -> QList [QSymbol "quasiquote", datumToQuote d]
    DUnquote d -> QList [QSymbol "unquote", datumToQuote d]
    DUnquoteSplicing d -> QList [QSymbol "unquote-splicing", datumToQuote d]

-- | Generate a fresh name (for internal use, deterministic).
_freshName :: Text -> Int -> Text
_freshName prefix n = prefix <> T.pack (show n)
