\ Stack manipulation and arithmetic

: square ( n -- n^2 )
  dup * ;

: cube ( n -- n^3 )
  dup dup * * ;

: abs ( n -- |n| )
  dup 0 < if negate then ;

: max ( a b -- max )
  2dup < if swap then drop ;

: min ( a b -- min )
  2dup > if swap then drop ;

variable counter
: reset-counter 0 counter ! ;
: increment counter @ 1 + counter ! ;
: show-counter counter @ . ;

: count-down ( n -- )
  begin
    dup 0 >
  while
    dup .
    1 -
  repeat
  drop ;

: fizzbuzz ( n -- )
  1 do
    i 15 mod 0 = if ." FizzBuzz" else
    i 3 mod 0 = if ." Fizz" else
    i 5 mod 0 = if ." Buzz" else
    i .
    then then then
    cr
  loop ;
