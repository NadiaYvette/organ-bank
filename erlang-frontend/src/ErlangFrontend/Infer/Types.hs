{- | Internal type representation for Erlang type inference.
Mirrors the SML frontend's Elab.Types, adapted for Erlang's type system.

Erlang is dynamically typed, so we infer what we can and leave the rest
as unresolved type variables (which become TAny in OrganIR emission).
-}
module ErlangFrontend.Infer.Types where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | Unique identifier for unification variables.
type TyUID = Int

-- | Internal types used during Erlang type inference.
data ErlType
    = -- | Unification variable
      TVar Int
    | -- | Type constructor: atom, integer, float, string, boolean, pid, ...
      TCon Text
    | -- | Parameterised type: list(T), map(K,V), etc.
      TApp Text [ErlType]
    | -- | Function type: args -> result
      TFun [ErlType] ErlType
    | -- | Tuple type: {T1, T2, ...}
      TTuple [ErlType]
    deriving (Eq, Show)

-- | Type scheme: forall a1..an. ty
data Scheme = Forall [Int] ErlType
    deriving (Show)

-- | Monomorphic scheme (no quantified variables).
monoScheme :: ErlType -> Scheme
monoScheme = Forall []

-- | Substitution: maps unification variables to types.
type Subst = Map Int ErlType

emptySubst :: Subst
emptySubst = Map.empty

-- | Apply substitution, chasing variable chains.
apply :: Subst -> ErlType -> ErlType
apply s (TVar u) = case Map.lookup u s of
    Just t -> apply s t
    Nothing -> TVar u
apply _ t@(TCon _) = t
apply s (TApp c args) = TApp c (map (apply s) args)
apply s (TFun args ret) = TFun (map (apply s) args) (apply s ret)
apply s (TTuple ts) = TTuple (map (apply s) ts)

applyScheme :: Subst -> Scheme -> Scheme
applyScheme s (Forall vs t) = Forall vs (apply (foldr Map.delete s vs) t)

-- | Free type variables.
ftv :: ErlType -> [Int]
ftv (TVar u) = [u]
ftv (TCon _) = []
ftv (TApp _ args) = concatMap ftv args
ftv (TFun args ret) = concatMap ftv args ++ ftv ret
ftv (TTuple ts) = concatMap ftv ts

ftvScheme :: Scheme -> [Int]
ftvScheme (Forall vs t) = filter (`notElem` vs) (ftv t)

-- * Built-in Erlang types

tyAtom, tyInteger, tyFloat, tyNumber, tyString, tyBoolean, tyPid, tyRef :: ErlType
tyAtom = TCon "atom"
tyInteger = TCon "integer"
tyFloat = TCon "float"
tyNumber = TCon "number"
tyString = TCon "string"
tyBoolean = TCon "boolean"
tyPid = TCon "pid"
tyRef = TCon "reference"

tyList :: ErlType -> ErlType
tyList t = TApp "list" [t]

tyTuple :: [ErlType] -> ErlType
tyTuple = TTuple
