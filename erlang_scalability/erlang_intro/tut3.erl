-module(tut3).
-export([start/0, ping/1, pong/0]).

ping(0) ->
  pong ! finished,
  io:format("Ping finished~n", []);

ping(N) ->
  pong ! {ping, self()},
  receive
    pong ->
      io:format("Ping received pong~n", [])
  end,
  ping(N-1).

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
  Pong_PID = spawn(tut3, pong, []),
  register(pong, Pong_PID),
  spawn(tut3, ping, [3]).