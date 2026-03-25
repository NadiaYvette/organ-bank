module hello where

open import Agda.Builtin.Nat

factorial : Nat → Nat
factorial zero = 1
factorial (suc n) = suc n * factorial n
