-module(macro).
-export([test_assign/0]).

-define(Assign(Var, Exp), Var=Exp,
  io:format("~s = ~s -> ~p~n", [??Var, ??Exp, Var])).

test_assign() ->
  ?Assign(X, lists:sum([1,2,3])),
  X.