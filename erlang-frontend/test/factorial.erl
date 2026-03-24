-module(factorial).
-export([fac/1, fac_tail/2]).

fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N - 1).

fac_tail(0, Acc) -> Acc;
fac_tail(N, Acc) when N > 0 -> fac_tail(N - 1, N * Acc).
