-module(frequency).
-behaviour(gen_server).

-export([start_link/0, allocate/0, deallocate/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([format_status/2]).

%% Client functions
start_link() ->
  process_flag(trap_exit, true),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{debug, [statistics, trace, log]}]).

allocate() ->
  gen_server:call(?MODULE, {allocate, self()}).

deallocate(Frequency) ->
  gen_server:cast(?MODULE, {deallocate, Frequency}).

stop() ->
  gen_server:cast(?MODULE, stop).

%% Callbacks
init(_Args) ->
  Frequencies = {get_frequencies(), []},
  {ok, Frequencies}.

handle_call({allocate, Pid}, _From, Frequencies) ->
  {NewFrequencies, Reply} =  allocate(Frequencies, Pid),
  {reply, Reply, NewFrequencies}.

handle_cast({deallocate, Freq}, Frequencies) ->
  NewFrequencies = deallocate(Frequencies, Freq),
  {noreply, NewFrequencies};
handle_cast(stop, LoopData) ->
  {stop, normal, LoopData}.

handle_info(_Info, LoopData) ->
  {noreply, LoopData}.

terminate(_Reason, _LoopData) ->
  ok.

format_status(_Opt, [_ProcDict, {Available, Allocated}]) ->
  {data, [{"State", {{available, Available}, {allocated, Allocated}}}]}.

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
      {Free, Allocated}
  end.