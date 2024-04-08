-module(ex3).
-export([filter/2, is_even/1, filter_list/2]).

filter(_P, []) -> [];
filter(P, [H|T]) ->
  case P(H) of
    true -> [H|filter(P, T)];
    false -> filter(P, T)
  end.

filter_list(P, List) ->
  [ X || X <- List, P(X)].

is_even(X) ->
  X rem 2 == 0.

