-- | Internal type representation for elaboration.
module SmlFrontend.Elab.Types where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | Unique identifier for unification variables.
type TyUID = Int

-- | Internal types used during elaboration.
data ITy
    = -- | Unification variable
      ITyVar TyUID
    | -- | Type constructor applied to args
      ITyCon String [ITy]
    | -- | Function type
      ITyFun ITy ITy
    | -- | Tuple type
      ITyTuple [ITy]
    | -- | Record type {lab: ty, ...}
      ITyRecord [(String, ITy)]
    deriving (Eq, Show)

-- | Type scheme: forall a1..an. ty
data Scheme = Scheme [TyUID] ITy
    deriving (Show)

monoScheme :: ITy -> Scheme
monoScheme = Scheme []

-- | Substitution.
type Subst = Map TyUID ITy

emptySubst :: Subst
emptySubst = Map.empty

-- | Apply substitution, following chains.
apply :: Subst -> ITy -> ITy
apply s (ITyVar u) = case Map.lookup u s of
    Just t -> apply s t
    Nothing -> ITyVar u
apply s (ITyCon c args) = ITyCon c (map (apply s) args)
apply s (ITyFun a b) = ITyFun (apply s a) (apply s b)
apply s (ITyTuple ts) = ITyTuple (map (apply s) ts)
apply s (ITyRecord fs) = ITyRecord [(l, apply s t) | (l, t) <- fs]

applyScheme :: Subst -> Scheme -> Scheme
applyScheme s (Scheme vs t) = Scheme vs (apply (foldr Map.delete s vs) t)

-- | Free unification variables.
ftv :: ITy -> [TyUID]
ftv (ITyVar u) = [u]
ftv (ITyCon _ args) = concatMap ftv args
ftv (ITyFun a b) = ftv a ++ ftv b
ftv (ITyTuple ts) = concatMap ftv ts
ftv (ITyRecord fs) = concatMap (ftv . snd) fs

ftvScheme :: Scheme -> [TyUID]
ftvScheme (Scheme vs t) = filter (`notElem` vs) (ftv t)

-- | Built-in type constructors.
tyInt, tyReal, tyString, tyChar, tyBool, tyUnit, tyExn :: ITy
tyInt = ITyCon "int" []
tyReal = ITyCon "real" []
tyString = ITyCon "string" []
tyChar = ITyCon "char" []
tyBool = ITyCon "bool" []
tyUnit = ITyTuple []
tyExn = ITyCon "exn" []

tyList :: ITy -> ITy
tyList t = ITyCon "list" [t]

tyRef :: ITy -> ITy
tyRef t = ITyCon "ref" [t]

tyOption :: ITy -> ITy
tyOption t = ITyCon "option" [t]
