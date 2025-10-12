-module(demo).
-export([double/1, times/2]).

double(Value) ->
  Value * 2.

times(X, Y) ->
  X * Y.
