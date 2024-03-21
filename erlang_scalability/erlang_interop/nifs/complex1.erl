-module(complex1).
-export([foo/1, bar/1]).
-nifs([foo/1, bar/1]).
-on_load(init/0).

init() ->
  ok = erlang:load_nif("./complex1_nif", 0).

foo(_X) ->
  erlang:nif_error(nif_library_not_found).

bar(_X) ->
  erlang:nif_error(nif_library_not_found).
