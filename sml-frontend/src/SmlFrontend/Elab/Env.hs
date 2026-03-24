-- | Type environments for elaboration.
module SmlFrontend.Elab.Env where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import SmlFrontend.Elab.Types

-- | Type environment: maps value identifiers to type schemes.
newtype TyEnv = TyEnv (Map String Scheme)
    deriving (Show)

emptyEnv :: TyEnv
emptyEnv = TyEnv Map.empty

lookupEnv :: String -> TyEnv -> Maybe Scheme
lookupEnv x (TyEnv m) = Map.lookup x m

extendEnv :: String -> Scheme -> TyEnv -> TyEnv
extendEnv x s (TyEnv m) = TyEnv (Map.insert x s m)

extendEnvMany :: [(String, Scheme)] -> TyEnv -> TyEnv
extendEnvMany bindings env = foldl (\e (x, s) -> extendEnv x s e) env bindings

envBindings :: TyEnv -> [(String, Scheme)]
envBindings (TyEnv m) = Map.toList m

applySubstEnv :: Subst -> TyEnv -> TyEnv
applySubstEnv s (TyEnv m) = TyEnv (Map.map (applyScheme s) m)

ftvEnv :: TyEnv -> [TyUID]
ftvEnv (TyEnv m) = concatMap ftvScheme (Map.elems m)

-- | Constructor environment: maps constructor names to (scheme, tycon name).
newtype ConEnv = ConEnv (Map String (Scheme, String))
    deriving (Show)

emptyConEnv :: ConEnv
emptyConEnv = ConEnv Map.empty

lookupCon :: String -> ConEnv -> Maybe (Scheme, String)
lookupCon c (ConEnv m) = Map.lookup c m

extendCon :: String -> Scheme -> String -> ConEnv -> ConEnv
extendCon c s tc (ConEnv m) = ConEnv (Map.insert c (s, tc) m)
