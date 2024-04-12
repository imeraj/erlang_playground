-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/1, terminate/1, handle/2]).

%% Client API
start() ->
  server:start(frequency, []).

stop() -> server:stop(frequency).
allocate() -> server:call(frequency, {allocate, self()}).
deallocate(Freq) -> server:call(frequency, {deallocate, Freq}).

%% callbacks
init(_Args) ->
  {get_frequencies(), []}.

handle({allocate, Pid}, Frequencies) ->
  allocate(Frequencies, Pid);
handle({deallocate, Freq}, Frequencies) ->
  case deallocate(Frequencies, Freq) of
    {NewFrequencies, {error, invalid_frequency}} ->
      {NewFrequencies, {error, invalid_frequency}};
    NewFrequencies ->
      {NewFrequencies, ok}
  end.

terminate(_Frequencies) ->
  ok.

%% Helper functions
get_frequencies() -> [10,11,12,13,14,15].

allocate({[], Allocated}, _Pid) -> {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid} | Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  case lists:keymember(Freq, 1, Allocated) of
    true ->
      NewAllocated = lists:keydelete(Freq, 1, Allocated),
      {[Freq|Free], NewAllocated};
    false ->
      {{Free, Allocated}, {error, invalid_frequency}}
  end.


