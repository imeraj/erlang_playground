% Compile with `erlc -S add.erl`
-module(add).
-export([add/2]).

add(A, B) ->
    id(A) + id(B).

id(I) ->
    I.
