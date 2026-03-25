fun factorial 0 = 1
  | factorial n = n * factorial (n - 1);

fun main () = factorial 10;
