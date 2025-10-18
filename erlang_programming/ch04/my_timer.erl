-module(my_timer).
-export([send_after/2, send/3, sleep/1]).

send_after(Time, Msg) ->
    spawn(my_timer, send, [self(), Time, Msg]).

send(From, Time, Msg) ->
    receive
    after
        Time ->
            From ! Msg
    end.

sleep(T) ->
    receive
    after
        T -> ok
    end.
