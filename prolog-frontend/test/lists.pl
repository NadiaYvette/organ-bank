append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).

member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).

length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

reverse(Xs, Ys) :- reverse(Xs, [], Ys).
reverse([], Acc, Acc).
reverse([X|Xs], Acc, Ys) :- reverse(Xs, [X|Acc], Ys).

last([X], X).
last([_|T], X) :- last(T, X).
