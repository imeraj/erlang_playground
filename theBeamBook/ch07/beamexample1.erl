-module(beamexample1).
-export([id/1]).

id(I) when is_integer(I) -> I.