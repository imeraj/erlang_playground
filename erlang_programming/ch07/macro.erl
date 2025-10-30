-module(macro).
-export([tstFun/2, test/0]).

-include("macros.hrl").

tstFun(Z,W) when ?MULTIPLE(Z,W) -> true;
tstFun(_Z, _W)                  -> false.

test() -> ?VALUE(length([1,2,3])).
