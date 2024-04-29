-module(bsc).
-behaviour(application).

-export([start/2, stop/1, start_phase/3]).

start(_StartType, _StartArgs) ->
  bsc_sup:start_link().

start_phase(StartPhase, StartType, Args) ->
  io:format("bsc:start_phase(~p,~p,~p).~n",
    [StartPhase, StartType, Args]).

stop(_Data) ->
  ok.