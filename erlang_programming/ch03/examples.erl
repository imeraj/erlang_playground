-module(examples).
-export([even/1, number/1, bump/1, filter_even/1, member/2, reverse/1, merge/2]).

even(Int) when Int rem 2 =:= 0 -> true;
even(Int) when Int rem 2 =:= 1 -> false.

number(Num) when is_integer(Num) -> integer;
number(Num) when is_float(Num) -> float;
number(_) -> false.

bump([]) -> [];
bump([H|T]) -> [H + 1 | bump(T)].

filter_even([]) -> [];
filter_even([H|T]) when H rem 2 =:= 0 -> [H | filter_even(T)];
filter_even([_|T]) -> filter_even(T).

member(_, []) -> false;
member(H, [H|_]) -> true;
member(H, [_|T]) -> member(H, T).

reverse(List) -> reverse_acc(List, []).

reverse_acc([], Acc) -> Acc;
reverse_acc([H|T], Acc) -> reverse_acc(T, [H | Acc]).

merge(Xs, Ys) ->
    lists:reverse(mergeL(Xs, Ys, [])).

mergeL([X|Xs], Ys, Zs) ->
    mergeR(Xs, Ys, [X | Zs]);
mergeL([], [], Zs) -> Zs.

mergeR(Xs, [Y|Ys], Zs) ->
    mergeL(Xs, Ys, [Y | Zs]);
mergeR([], [], Zs) -> Zs.
