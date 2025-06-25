-module(mini_proc).
-export([wait/0]).

wait() ->
    X = {timestamp, erlang:timestamp()},
    receive _ -> X end,
    X.
