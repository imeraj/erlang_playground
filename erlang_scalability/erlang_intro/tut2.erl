-module(tut2).
-export([start/0, ping/2, pong/0]).

-define(COUNT, 3).

ping(0, Pong_PID) ->
  Pong_PID ! finished,
  io:format("Ping finished~n", []);

ping(N, Pong_PID) ->
  Pong_PID ! {ping, self()},
  receive
    pong ->
      io:format("Ping received pong~n", [])
  end,
  ping(N-1, Pong_PID).

pong() ->
  receive
    finished ->
      io:format("Pong finished~n", []);

    {ping, Ping_PID} ->
      io:format("Pong received ping~n", []),
      Ping_PID ! pong,
      pong()
  end.

start() ->
  Pong_PID = spawn(tut2, pong, []),
  spawn(tut2, ping, [?COUNT, Pong_PID]).