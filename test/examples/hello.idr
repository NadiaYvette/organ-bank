module Hello

factorial : Nat -> Nat
factorial Z = 1
factorial (S n) = (S n) * factorial n
