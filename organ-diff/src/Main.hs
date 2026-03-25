{- | organ-diff: structural comparison of two OrganIR JSON documents.
Ignores name unique suffixes and metadata differences, focusing on
definition-level changes.
-}
module Main (main) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OrganIR.Parse (parseOrganIR)
import OrganIR.Types
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    args <- getArgs
    (fileA, fileB) <- case args of
        [a, b] -> pure (a, b)
        _ -> do
            hPutStrLn stderr "Usage: organ-diff <file1.json> <file2.json>"
            exitFailure
    irA <- loadIR fileA
    irB <- loadIR fileB
    let diffs = diffOrganIR irA irB
    mapM_ (TIO.putStrLn . renderDiff) diffs
    if null diffs
        then exitSuccess
        else exitFailure

loadIR :: FilePath -> IO OrganIR
loadIR path = do
    txt <- TIO.readFile path
    case parseOrganIR txt of
        Left err -> do
            hPutStrLn stderr $ path ++ ": parse error: " ++ T.unpack err
            exitFailure
        Right ir -> pure ir

-- * Diff types

data Diff
    = DefAdded Text
    | DefRemoved Text
    | DefChanged Text [FieldDiff]
    deriving (Show)

data FieldDiff
    = TypeChanged
    | BodyChanged
    | ArityChanged Int Int
    | VisibilityChanged Visibility Visibility
    | SortChanged Sort Sort
    deriving (Show)

renderDiff :: Diff -> Text
renderDiff = \case
    DefAdded n -> "+ " <> n
    DefRemoved n -> "- " <> n
    DefChanged n fds -> "~ " <> n <> "\n" <> T.unlines (map (("    " <>) . renderFieldDiff) fds)

renderFieldDiff :: FieldDiff -> Text
renderFieldDiff = \case
    TypeChanged -> "type changed"
    BodyChanged -> "body changed"
    ArityChanged a b -> "arity: " <> T.pack (show a) <> " -> " <> T.pack (show b)
    VisibilityChanged a b -> "visibility: " <> showVis a <> " -> " <> showVis b
    SortChanged a b -> "sort: " <> showSort a <> " -> " <> showSort b

showVis :: Visibility -> Text
showVis Public = "public"
showVis Private = "private"

showSort :: Sort -> Text
showSort SFun = "fun"
showSort SVal = "val"
showSort SExternal = "external"
showSort SCon = "con"

-- * Structural diff

diffOrganIR :: OrganIR -> OrganIR -> [Diff]
diffOrganIR a b =
    let defsA = modDefs (irModule a)
        defsB = modDefs (irModule b)
        keysA = map defKey defsA
        keysB = map defKey defsB
        -- Definitions present only in A (removed)
        removed = [DefRemoved k | k <- keysA, k `notElem` keysB]
        -- Definitions present only in B (added)
        added = [DefAdded k | k <- keysB, k `notElem` keysA]
        -- Definitions in both: compare
        common =
            [ case diffDef da db of
                [] -> Nothing
                fds -> Just (DefChanged k fds)
            | k <- keysA
            , k `elem` keysB
            , let da = findDef k defsA
                  db = findDef k defsB
            ]
     in removed ++ added ++ [d | Just d <- common]

-- | Canonical key for a definition: module + name text (ignoring unique).
defKey :: Definition -> Text
defKey d =
    let qn = defName d
     in qnModule qn <> "." <> nameText (qnName qn)

findDef :: Text -> [Definition] -> Definition
findDef k defs = case [d | d <- defs, defKey d == k] of
    (d : _) -> d
    [] -> error $ "findDef: impossible, key not found: " ++ T.unpack k

diffDef :: Definition -> Definition -> [FieldDiff]
diffDef a b =
    [TypeChanged | not (eqTy (defType a) (defType b))]
        ++ [BodyChanged | not (eqExpr (defExpr a) (defExpr b))]
        ++ [ArityChanged (defArity a) (defArity b) | defArity a /= defArity b]
        ++ [VisibilityChanged (defVisibility a) (defVisibility b) | defVisibility a /= defVisibility b]
        ++ [SortChanged (defSort a) (defSort b) | defSort a /= defSort b]

-- * Structural equality ignoring unique suffixes

-- | Compare names ignoring unique counter.
eqName :: Name -> Name -> Bool
eqName a b = nameText a == nameText b

eqQName :: QName -> QName -> Bool
eqQName a b = qnModule a == qnModule b && eqName (qnName a) (qnName b)

eqTy :: Ty -> Ty -> Bool
eqTy (TForall vs1 t1) (TForall vs2 t2) =
    length vs1 == length vs2
        && all (\(a, b) -> eqName (tvName a) (tvName b) && tvKind a == tvKind b) (zip vs1 vs2)
        && eqTy t1 t2
eqTy (TFn args1 eff1 r1) (TFn args2 eff2 r2) =
    length args1 == length args2
        && all (\(a, b) -> fnArgMultiplicity a == fnArgMultiplicity b && eqTy (fnArgType a) (fnArgType b)) (zip args1 args2)
        && eqEffectRow eff1 eff2
        && eqTy r1 r2
eqTy (TApp c1 ts1) (TApp c2 ts2) =
    eqQName c1 c2 && length ts1 == length ts2 && all (uncurry eqTy) (zip ts1 ts2)
eqTy (TCon q1) (TCon q2) = eqQName q1 q2
eqTy (TVar n1) (TVar n2) = eqName n1 n2
eqTy (TSyn n1 e1) (TSyn n2 e2) = eqQName n1 n2 && eqTy e1 e2
eqTy TAny TAny = True
eqTy _ _ = False

eqEffectRow :: EffectRow -> EffectRow -> Bool
eqEffectRow a b =
    length (erEffects a) == length (erEffects b)
        && all (uncurry eqQName) (zip (erEffects a) (erEffects b))
        && case (erTail a, erTail b) of
            (Nothing, Nothing) -> True
            (Just x, Just y) -> eqName x y
            _ -> False

eqExpr :: Expr -> Expr -> Bool
eqExpr (EVar n1) (EVar n2) = eqName n1 n2
eqExpr (ELit l1) (ELit l2) = eqLit l1 l2
eqExpr (ECon q1 es1) (ECon q2 es2) = eqQName q1 q2 && eqExprs es1 es2
eqExpr (EApp f1 as1) (EApp f2 as2) = eqExpr f1 f2 && eqExprs as1 as2
eqExpr (ELam ps1 b1) (ELam ps2 b2) =
    length ps1 == length ps2
        && all (\(a, b) -> eqName (lpName a) (lpName b)) (zip ps1 ps2)
        && eqExpr b1 b2
eqExpr (ELet bs1 b1) (ELet bs2 b2) =
    length bs1 == length bs2
        && all (\(a, b) -> eqName (lbName a) (lbName b) && eqExpr (lbExpr a) (lbExpr b)) (zip bs1 bs2)
        && eqExpr b1 b2
eqExpr (ECase s1 br1) (ECase s2 br2) =
    eqExpr s1 s2
        && length br1 == length br2
        && all (\(a, b) -> eqPat (brPattern a) (brPattern b) && eqExpr (brBody a) (brBody b)) (zip br1 br2)
eqExpr (ETypeApp e1 ts1) (ETypeApp e2 ts2) =
    eqExpr e1 e2 && length ts1 == length ts2 && all (uncurry eqTy) (zip ts1 ts2)
eqExpr (ETypeLam vs1 b1) (ETypeLam vs2 b2) =
    length vs1 == length vs2
        && all (\(a, b) -> eqName (tvName a) (tvName b)) (zip vs1 vs2)
        && eqExpr b1 b2
eqExpr (EPerform q1 op1 as1) (EPerform q2 op2 as2) =
    eqQName q1 q2 && op1 == op2 && eqExprs as1 as2
eqExpr (EHandle q1 b1 h1) (EHandle q2 b2 h2) =
    eqQName q1 q2 && eqExpr b1 b2 && eqExpr h1 h2
eqExpr (ERetain n1) (ERetain n2) = eqName n1 n2
eqExpr (ERelease n1) (ERelease n2) = eqName n1 n2
eqExpr (EDrop n1) (EDrop n2) = eqName n1 n2
eqExpr (EReuse n1) (EReuse n2) = eqName n1 n2
eqExpr (EDelay e1) (EDelay e2) = eqExpr e1 e2
eqExpr (EForce e1) (EForce e2) = eqExpr e1 e2
eqExpr (ETuple es1) (ETuple es2) = eqExprs es1 es2
eqExpr (EList es1) (EList es2) = eqExprs es1 es2
eqExpr (ERaise e1) (ERaise e2) = eqExpr e1 e2
eqExpr EUnreachable EUnreachable = True
eqExpr _ _ = False

eqExprs :: [Expr] -> [Expr] -> Bool
eqExprs as bs = length as == length bs && all (uncurry eqExpr) (zip as bs)

eqLit :: Lit -> Lit -> Bool
eqLit (LitInt a) (LitInt b) = a == b
eqLit (LitFloat a) (LitFloat b) = a == b
eqLit (LitString a) (LitString b) = a == b
eqLit (LitBool a) (LitBool b) = a == b
eqLit _ _ = False

eqPat :: Pat -> Pat -> Bool
eqPat (PatCon q1 bs1) (PatCon q2 bs2) =
    eqQName q1 q2
        && length bs1 == length bs2
        && all (\(a, b) -> eqName (pbName a) (pbName b)) (zip bs1 bs2)
eqPat (PatLit l1) (PatLit l2) = eqLit l1 l2
eqPat (PatVar n1 _) (PatVar n2 _) = eqName n1 n2
eqPat PatWild PatWild = True
eqPat _ _ = False
