: factorial ( n -- n! ) dup 1 > if dup 1 - recurse * then ;
