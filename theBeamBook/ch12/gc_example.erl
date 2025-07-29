-module(gc_example).
-export([example/0]).

example() ->
    T = gen_data(),
    S = element(1, T),
    erlang:garbage_collect(),
    S.

gen_data() ->
    S = gen_string($H, $E, $L, $L, $O),
    T = gen_tuple([S, S], S),
    T.

gen_string(A,B,C,D,E) ->
    [A,B,C,D,E].

gen_tuple(A, B) ->
    {A,B}.