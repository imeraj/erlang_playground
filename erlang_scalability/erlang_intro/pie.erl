-module(pie).
-export([pie/0, circumference/1]).

-define(PI, 3.14).

pie() -> 3.14.

circumference(Radius) ->
  2 * ?PI * Radius.