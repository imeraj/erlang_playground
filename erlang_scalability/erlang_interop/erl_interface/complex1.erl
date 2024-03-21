-module(complex1).
-export([start/1, stop/0, init/1]).
-export([foo/1, bar/1]).

start(ExtPrg) ->
  spawn(?MODULE, init, [ExtPrg]).

stop() ->
  complex1 ! stop.

foo(X) ->
  call_port({foo, X}).

bar(Y) ->
  call_port({bar, Y}).

call_port(Msg) ->
  complex1 ! {call, self(), Msg},
  receive
    {complex1, Result} ->
      Result
  end.

init(ExtPrg) ->
  register(complex1, self()),
  process_flag(trap_exit, true),
  Port = open_port({spawn,ExtPrg}, [{packet, 2}, binary]),
  loop(Port).

loop(Port) ->
  receive
    {call, Caller, Msg} ->
      Port ! {self(), {command, term_to_binary(Msg)}},
      receive
        {Port, {data, Data}} ->
          Caller ! {complex1, binary_to_term(Data)}
      end,
      loop(Port);
    stop ->
      Port ! {self(), close},
      receive
        {Port, closed} ->
          exit(normal)
      end;
    {'EXIT', Port, _Reason} ->
      exit(port_terminated)
  end.

