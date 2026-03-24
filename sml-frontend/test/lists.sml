datatype 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

fun depth Leaf = 0
  | depth (Node (l, _, r)) =
      let
        val dl = depth l
        val dr = depth r
      in
        if dl > dr then 1 + dl else 1 + dr
      end

val xs = [1, 2, 3, 4, 5]
val total = foldl (fn (x, acc) => x + acc) 0 xs
