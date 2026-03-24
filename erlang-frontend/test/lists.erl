-module(lists).
-export([len/1, map/2, filter/2, reverse/1, append/2]).

len([]) -> 0;
len([_ | T]) -> 1 + len(T).

map(_, []) -> [];
map(F, [H | T]) -> [F(H) | map(F, T)].

filter(_, []) -> [];
filter(F, [H | T]) ->
    case F(H) of
        true -> [H | filter(F, T)];
        false -> filter(F, T)
    end.

reverse(L) -> reverse(L, []).
reverse([], Acc) -> Acc;
reverse([H | T], Acc) -> reverse(T, [H | Acc]).

append([], B) -> B;
append([H | T], B) -> [H | append(T, B)].
