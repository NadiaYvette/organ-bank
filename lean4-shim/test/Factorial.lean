def factorial : Nat → Nat
  | 0 => 1
  | n + 1 => (n + 1) * factorial n

def main : IO Unit :=
  IO.println s!"{factorial 10}"
