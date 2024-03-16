-module(tut4).
-export([start_pong/0, start_ping/1,  ping/2, pong/0]).

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
  after 5000 ->
    io:format("Pong timeout~n", [])
  end.

start_pong() ->
  Pong_PID = spawn(tut4, pong, []),
  register(pong, Pong_PID).

start_ping(Pong_Node) ->
  spawn(tut4, ping, [3, Pong_Node]).