-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

%% Client functions
start() ->
  register(frequency, spawn(frequency, init, [])).

stop() -> call(stop).
allocate() -> call(allocate).
deallocate(Frequency) -> call({deallocate, Frequency}).

%% Server functions
init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid, {deallocate, Frequency}} ->
      case deallocate(Frequencies, Frequency) of
        {NewFrequencies, {error, invalid_frequency}} ->
          reply(Pid, {error, invalid_frequency}),
          loop(NewFrequencies);
        NewFrequencies ->
          reply(Pid, ok),
          loop(NewFrequencies)
      end;
    {request, Pid, stop} ->
      reply(Pid, ok)
  end.

reply(Pid, Reply) ->
  Pid ! {reply, Reply}.

%% Internal Client functions
call(Message) ->
  frequency ! {request, self(), Message},
  receive
    {reply, Reply} ->
      Reply
  end.

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


