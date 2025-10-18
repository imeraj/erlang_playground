-module(echo).
-export([go/0, loop/0]).

go() ->
    register(echo, spawn(echo, loop, [])),
    echo ! {self(), hello},
    receive
        {_From, Msg} ->
            io:format("~w~n", [Msg])
    end,
    echo ! stop.

loop() ->
    receive
        {From, Msg} ->
            From ! {self(), Msg},
            loop();
        stop ->
            true
    end.
