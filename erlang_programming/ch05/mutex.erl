-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
    register(mutex, spawn(?MODULE, init, [])),
    ok.

stop() ->
    mutex ! stop.

wait() ->
    mutex ! {wait, self()},
    receive ok -> ok end.

signal() ->
    mutex ! {signal, self()}, ok.

init() ->
    io:format("initialized~n"),
    free().

free() ->
    io:format("free~n"),
    receive
        {wait, From} ->
            From ! ok,
            busy(From);
        stop ->
            terminate()
    end.

busy(From) ->
    io:format("busy~n"),
    receive
        {signal, From} ->
            free()
    end.

terminate() ->
    io:format("terminated~n"),
    receive
        {wait, From} ->
            exit(From, kill),
            terminate()
    after
        0 -> ok
    end.
