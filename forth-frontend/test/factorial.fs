\ Factorial in Forth

: factorial ( n -- n! )
  dup 1 <= if
    drop 1
  else
    dup 1 - recurse *
  then ;

: factorial-iter ( n -- n! )
  1 swap
  begin
    dup 1 >
  while
    swap over * swap
    1 -
  repeat
  drop ;

: main
  10 factorial .
  10 factorial-iter . ;
