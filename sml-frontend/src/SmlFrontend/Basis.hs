{- | Initial basis for Standard ML (Definition Ch. 6, Appendix D).
Provides built-in types, constructors, and functions.
-}
module SmlFrontend.Basis (initialEnv, initialConEnv) where

import SmlFrontend.Elab.Env
import SmlFrontend.Elab.Types

-- | Initial type environment with built-in values.
initialEnv :: TyEnv
initialEnv = extendEnvMany bindings emptyEnv
  where
    -- Fresh variable UIDs starting at 1000 to avoid clashing with inference
    a = ITyVar 1000
    b = ITyVar 1001
    bindings =
        -- Arithmetic (int)
        [ ("+", Scheme [] (ITyFun (ITyTuple [tyInt, tyInt]) tyInt))
        , ("-", Scheme [] (ITyFun (ITyTuple [tyInt, tyInt]) tyInt))
        , ("*", Scheme [] (ITyFun (ITyTuple [tyInt, tyInt]) tyInt))
        , ("div", Scheme [] (ITyFun (ITyTuple [tyInt, tyInt]) tyInt))
        , ("mod", Scheme [] (ITyFun (ITyTuple [tyInt, tyInt]) tyInt))
        , ("~", Scheme [] (ITyFun tyInt tyInt))
        , ("abs", Scheme [] (ITyFun tyInt tyInt))
        , -- Comparisons (polymorphic over equality types, simplified)
          ("<", Scheme [] (ITyFun (ITyTuple [tyInt, tyInt]) tyBool))
        , (">", Scheme [] (ITyFun (ITyTuple [tyInt, tyInt]) tyBool))
        , ("<=", Scheme [] (ITyFun (ITyTuple [tyInt, tyInt]) tyBool))
        , (">=", Scheme [] (ITyFun (ITyTuple [tyInt, tyInt]) tyBool))
        , ("=", Scheme [1000] (ITyFun (ITyTuple [a, a]) tyBool))
        , ("<>", Scheme [1000] (ITyFun (ITyTuple [a, a]) tyBool))
        , -- String
          ("^", Scheme [] (ITyFun (ITyTuple [tyString, tyString]) tyString))
        , ("size", Scheme [] (ITyFun tyString tyInt))
        , ("str", Scheme [] (ITyFun tyChar tyString))
        , -- List operations
          ("::", Scheme [1000] (ITyFun (ITyTuple [a, tyList a]) (tyList a)))
        , ("@", Scheme [1000] (ITyFun (ITyTuple [tyList a, tyList a]) (tyList a)))
        , ("nil", Scheme [1000] (tyList a))
        , ("hd", Scheme [1000] (ITyFun (tyList a) a))
        , ("tl", Scheme [1000] (ITyFun (tyList a) (tyList a)))
        , ("null", Scheme [1000] (ITyFun (tyList a) tyBool))
        , ("length", Scheme [1000] (ITyFun (tyList a) tyInt))
        , ("rev", Scheme [1000] (ITyFun (tyList a) (tyList a)))
        , ("map", Scheme [1000, 1001] (ITyFun (ITyFun a b) (ITyFun (tyList a) (tyList b))))
        , ("foldl", Scheme [1000, 1001] (ITyFun (ITyFun (ITyTuple [a, b]) b) (ITyFun b (ITyFun (tyList a) b))))
        , ("foldr", Scheme [1000, 1001] (ITyFun (ITyFun (ITyTuple [a, b]) b) (ITyFun b (ITyFun (tyList a) b))))
        , -- Ref operations
          ("ref", Scheme [1000] (ITyFun a (tyRef a)))
        , ("!", Scheme [1000] (ITyFun (tyRef a) a))
        , (":=", Scheme [1000] (ITyFun (ITyTuple [tyRef a, a]) tyUnit))
        , -- IO
          ("print", Scheme [] (ITyFun tyString tyUnit))
        , -- Misc
          ("not", Scheme [] (ITyFun tyBool tyBool))
        , ("ignore", Scheme [1000] (ITyFun a tyUnit))
        , ("before", Scheme [1000, 1001] (ITyFun (ITyTuple [a, b]) a))
        , -- Conversions
          ("Int.toString", Scheme [] (ITyFun tyInt tyString))
        , ("Real.toString", Scheme [] (ITyFun tyReal tyString))
        ]

-- | Initial constructor environment.
initialConEnv :: ConEnv
initialConEnv = foldl (\env (c, s, tc) -> extendCon c s tc env) emptyConEnv ctors
  where
    a = ITyVar 1000
    ctors =
        -- bool
        [ ("true", Scheme [] tyBool, "bool")
        , ("false", Scheme [] tyBool, "bool")
        , -- list
          ("::", Scheme [1000] (ITyFun (ITyTuple [a, tyList a]) (tyList a)), "list")
        , ("nil", Scheme [1000] (tyList a), "list")
        , -- option
          ("SOME", Scheme [1000] (ITyFun a (tyOption a)), "option")
        , ("NONE", Scheme [1000] (tyOption a), "option")
        ]
