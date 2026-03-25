-- | Composable IR→IR optimization passes for OrganIR.
module OrganIR.Transform
    ( -- * Core infrastructure
      Pass
    , runPasses
    , mapDefs
    , mapExprs
    , foldExpr

      -- * Optimization passes
    , constantFold
    , deadDefElim
    , inlineTrivial
    , simplifyCase
    , etaReduce
    ) where

import Data.Text (Text)
import Data.Set (Set)
import Data.Set qualified as Set
import OrganIR.Types

-- * Core infrastructure

-- | A pass is a pure function from OrganIR to OrganIR.
type Pass = OrganIR -> OrganIR

-- | Compose passes left to right.
runPasses :: [Pass] -> OrganIR -> OrganIR
runPasses = foldl' (flip (.)) id
  -- foldl' (flip (.)) id [f, g, h] = h . g . f
  -- so applying the result to x gives h (g (f x)), i.e. left-to-right.

-- | Apply a function to every definition in the module.
mapDefs :: (Definition -> Definition) -> Pass
mapDefs f ir = ir{irModule = m{modDefs = map f (modDefs m)}}
  where
    m = irModule ir

-- | Apply a transformation to every expression in a definition (bottom-up).
mapExprs :: (Expr -> Expr) -> Definition -> Definition
mapExprs f def = def{defExpr = go (defExpr def)}
  where
    go expr = f $ case expr of
        EVar{}       -> expr
        ELit{}       -> expr
        ECon qn es   -> ECon qn (map go es)
        EApp fn args -> EApp (go fn) (map go args)
        ELam ps body -> ELam ps (go body)
        ELet bs body -> ELet (map goBind bs) (go body)
        ECase s brs  -> ECase (go s) (map goBranch brs)
        ETypeApp e ts -> ETypeApp (go e) ts
        ETypeLam tvs e -> ETypeLam tvs (go e)
        EPerform qn op args -> EPerform qn op (map go args)
        EHandle qn h b -> EHandle qn (go h) (go b)
        ERetain{}    -> expr
        ERelease{}   -> expr
        EDrop{}      -> expr
        EReuse{}     -> expr
        EDelay e     -> EDelay (go e)
        EForce e     -> EForce (go e)
        ETuple es    -> ETuple (map go es)
        EList es     -> EList (map go es)
        ERaise e     -> ERaise (go e)
        EUnreachable -> expr

    goBind lb = lb{lbExpr = go (lbExpr lb)}
    goBranch br = br{brBody = go (brBody br)}

-- | Fold over all subexpressions (pre-order).
foldExpr :: (Expr -> a -> a) -> a -> Expr -> a
foldExpr f z expr = f expr $ case expr of
    EVar{}       -> z
    ELit{}       -> z
    ECon _ es    -> foldr (flip (foldExpr f)) z es
    EApp fn args -> foldr (flip (foldExpr f)) (foldExpr f z fn) args
    ELam _ body  -> foldExpr f z body
    ELet bs body -> foldExpr f (foldr (\lb acc -> foldExpr f acc (lbExpr lb)) z bs) body
    ECase s brs  -> foldr (\br acc -> foldExpr f acc (brBody br)) (foldExpr f z s) brs
    ETypeApp e _ -> foldExpr f z e
    ETypeLam _ e -> foldExpr f z e
    EPerform _ _ args -> foldr (flip (foldExpr f)) z args
    EHandle _ h b -> foldExpr f (foldExpr f z h) b
    ERetain{}    -> z
    ERelease{}   -> z
    EDrop{}      -> z
    EReuse{}     -> z
    EDelay e     -> foldExpr f z e
    EForce e     -> foldExpr f z e
    ETuple es    -> foldr (flip (foldExpr f)) z es
    EList es     -> foldr (flip (foldExpr f)) z es
    ERaise e     -> foldExpr f z e
    EUnreachable -> z

-- * Optimization passes

-- | Fold constant arithmetic and simplify known-scrutinee case expressions.
--
-- Handles: +, -, * for integers and floats.
-- Also folds: @ECase (ELit (LitBool True)) [Branch PatTrue body, ...]@ to @body@.
constantFold :: Pass
constantFold = mapDefs (mapExprs go)
  where
    go (EApp (EVar n) [ELit (LitInt a), ELit (LitInt b)])
        | nameText n == "+" = ELit (LitInt (a + b))
        | nameText n == "-" = ELit (LitInt (a - b))
        | nameText n == "*" = ELit (LitInt (a * b))
    go (EApp (EVar n) [ELit (LitFloat a), ELit (LitFloat b)])
        | nameText n == "+" = ELit (LitFloat (a + b))
        | nameText n == "-" = ELit (LitFloat (a - b))
        | nameText n == "*" = ELit (LitFloat (a * b))
    go (ECase (ELit (LitBool True)) brs)
        | Just body <- findTrueBranch brs = body
    go (ECase (ELit (LitBool False)) brs)
        | Just body <- findFalseBranch brs = body
    go e = e

    findTrueBranch [] = Nothing
    findTrueBranch (Branch (PatCon qn []) body : _)
        | qnName qn == Name "true" 0 = Just body
    findTrueBranch (Branch PatWild body : _) = Just body
    findTrueBranch (_ : rest) = findTrueBranch rest

    findFalseBranch [] = Nothing
    findFalseBranch (Branch (PatCon qn []) body : _)
        | qnName qn == Name "false" 0 = Just body
    findFalseBranch (Branch PatWild body : _) = Just body
    findFalseBranch (_ : rest) = findFalseBranch rest

-- | Remove unreferenced private definitions.
--
-- Computes the transitive closure of names referenced from public definitions,
-- then removes any private definition not in that set.
deadDefElim :: Pass
deadDefElim ir = ir{irModule = m{modDefs = filter keep (modDefs m)}}
  where
    m = irModule ir
    defs = modDefs m

    -- All names referenced from public definitions (transitive closure).
    reachable = transitiveClosure publicNames allRefs

    publicNames :: Set NameKey
    publicNames = Set.fromList
        [ defKey d | d <- defs, defVisibility d == Public ]

    -- Map from def name to the set of names it references.
    allRefs :: NameKey -> Set NameKey
    allRefs nk = case lookup nk [(defKey d, refsInExpr (defExpr d)) | d <- defs] of
        Just s  -> s
        Nothing -> Set.empty

    keep d
        | defVisibility d == Public = True
        | otherwise = defKey d `Set.member` reachable

-- | Inline trivial definitions (body is a single variable or literal).
--
-- Only inlines private definitions. After inlining, the definition is removed.
inlineTrivial :: Pass
inlineTrivial ir = ir{irModule = m{modDefs = remaining}}
  where
    m = irModule ir
    defs = modDefs m

    -- Find trivial private definitions.
    trivials :: [(Name, Expr)]
    trivials =
        [ (qnName (defName d), defExpr d)
        | d <- defs
        , defVisibility d == Private
        , isTrivial (defExpr d)
        ]

    trivialNames :: Set NameKey
    trivialNames = Set.fromList [nameKey n | (n, _) <- trivials]

    -- Substitute all trivial bindings in every remaining definition.
    remaining =
        [ mapExprs (substTrivials trivials) d
        | d <- defs
        , not (defKey d `Set.member` trivialNames)
        ]

    isTrivial (EVar _) = True
    isTrivial (ELit _) = True
    isTrivial _        = False

-- | Simplify degenerate case expressions.
--
-- * Single wildcard branch: @ECase scrut [Branch PatWild body]@ becomes @body@.
-- * Known constructor: @ECase (ECon qn []) [Branch (PatCon qn' []) body, ...]@
--   where @qn == qn'@ becomes @body@.
simplifyCase :: Pass
simplifyCase = mapDefs (mapExprs go)
  where
    go (ECase _ [Branch PatWild body]) = body
    go (ECase (ECon qn []) brs)
        | Just body <- findConBranch qn brs = body
    go e = e

    findConBranch _ [] = Nothing
    findConBranch qn (Branch (PatCon qn' []) body : _)
        | qn == qn' = Just body
    findConBranch _qn (Branch PatWild body : _) = Just body
    findConBranch qn (_ : rest) = findConBranch qn rest

-- | Eta-reduce lambdas.
--
-- @ELam [x] (EApp f [EVar x])@ where @x@ is not free in @f@ becomes @f@.
-- Only applies to single-parameter lambdas with single-argument applications.
etaReduce :: Pass
etaReduce = mapDefs (mapExprs go)
  where
    go (ELam [p] (EApp f [EVar v]))
        | lpName p == v
        , not (nameKey v `Set.member` freeVars f)
        = f
    go e = e

-- * Internal helpers

-- | A comparable key for names (using Text + unique).
type NameKey = (Text, Int)

nameKey :: Name -> NameKey
nameKey (Name t u) = (t, u)

defKey :: Definition -> NameKey
defKey = nameKey . qnName . defName

-- | Collect all free variable name keys in an expression.
freeVars :: Expr -> Set NameKey
freeVars = foldExpr collect Set.empty
  where
    collect (EVar n) acc = Set.insert (nameKey n) acc
    collect _ acc = acc

-- | Collect all referenced name keys in an expression.
refsInExpr :: Expr -> Set NameKey
refsInExpr = foldExpr collect Set.empty
  where
    collect (EVar n) acc = Set.insert (nameKey n) acc
    collect (EApp (EVar n) _) acc = Set.insert (nameKey n) acc
    collect _ acc = acc

-- | Compute the transitive closure of a set given a successor function.
transitiveClosure :: Set NameKey -> (NameKey -> Set NameKey) -> Set NameKey
transitiveClosure initial successors = go initial (Set.toList initial)
  where
    go visited [] = visited
    go visited (x : xs) =
        let new = Set.filter (`Set.notMember` visited) (successors x)
         in go (Set.union visited new) (xs ++ Set.toList new)

-- | Substitute trivial bindings in an expression.
substTrivials :: [(Name, Expr)] -> Expr -> Expr
substTrivials subs (EVar n)
    | Just replacement <- lookup n subs = replacement
substTrivials _ e = e
