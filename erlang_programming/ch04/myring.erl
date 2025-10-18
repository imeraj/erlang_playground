-module(myring).
-export([start/1,start_proc/2]).

start(Num) when Num >= 0 ->
    start_proc(Num, self()).

start_proc(0, From) ->
    From ! ok;
start_proc(Num, From) ->
    NPid = spawn(?MODULE, start_proc, [Num-1, From]),
    NPid ! ok,
    receive
        ok -> ok
    end.
