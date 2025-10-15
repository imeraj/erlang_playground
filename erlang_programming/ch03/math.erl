-module(math).
-export([add/2]).

add(X, Y) ->
    test_int(X),
    test_int(Y),
    X+Y.

test_int(X) when is_integer(X) -> true;
test_int(X) ->
    throw({error, {non_integer, X}}).
