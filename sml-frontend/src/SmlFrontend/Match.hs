{- | Pattern match compilation.
Compiles nested patterns to flat case trees (Maranget's algorithm, simplified).
For now, this is a simple direct translation — full exhaustiveness/redundancy
checking and optimal decision trees are future work.
-}
module SmlFrontend.Match (compileMatch) where

-- | A compiled decision tree.
data Decision
    = -- | Jump to clause N
      DLeaf Int
    | -- | Switch on variable
      DSwitch String [(String, Decision)] (Maybe Decision)
    deriving (Show)

{- | Compile a match (list of pattern-clause pairs) into a decision tree.
For now, this is a trivial sequential check.
-}
compileMatch :: Int -> Decision
compileMatch nClauses = foldr mkTest (DLeaf (nClauses - 1)) [0 .. nClauses - 2]
  where
    mkTest i _fallback = DLeaf i -- trivial: just try each clause in order
