-module(tut5).
-export([start/1,  ping/2, pong/0]).

ping(0, Pong_Node) ->
  {pong, Pong_Node} ! finished,
  io:format("Ping finished~n", []);

ping(N, Pong_Node) ->
%%  a tuple {registered_name, node_name} is used
  {pong, Pong_Node} ! {ping, self()},
  receive
    pong ->
      io:format("Ping received pong~n", [])
  end,
  ping(N-1, Pong_Node).

pong() ->
  receive
    finished ->
      io:format("Pong finished~n", []);

    {ping, Ping_PID} ->
      io:format("Pong received ping~n", []),
      Ping_PID ! pong,
      pong()
  end.

start(Ping_Node) ->
  Pong_PID = spawn(tut5, pong, []),
  register(pong, Pong_PID),
%%  I/O system finds out where the process is spawned from and sends all output there.
  spawn(Ping_Node, tut5, ping, [3, node()]).