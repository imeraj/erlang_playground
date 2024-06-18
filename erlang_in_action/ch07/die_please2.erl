-module(die_please2).

-export([start/0]).

-define(SLEEP_TIME, 2000).

start() ->
  proc_lib:spawn(fun go/0).

go() ->
  %% just sleep for a while, then crash
  timer:sleep(?SLEEP_TIME),
  i_really_want_to_die = right_now.