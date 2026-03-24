fun factorial 0 = 1
  | factorial n = n * factorial (n - 1)

val result = factorial 10
