-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

%% Client API
start() ->
    Pid = spawn(?MODULE, init, []),
    register(?MODULE, Pid),
    {ok, Pid}.

stop() -> call(stop).
allocate() -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

%% Server functions/callbacks
init() ->
    process_flag(trap_exit, true),
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

call(Message) ->
    frequency ! {request, self(), Message},
    receive
        {reply, Reply} -> Reply
    end.

loop(Frequencies) ->
    receive
        {request, From, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, From),
            reply(From, Reply),
            loop(NewFrequencies);
        {request, From, {deallocate, Freq}} ->
            {NewFrequencies, Reply} = deallocate(Frequencies, Freq),
            reply(From, Reply),
            loop(NewFrequencies);
        {'EXIT', Pid, _Reason} ->
            NewFrequencies = handle_exit(Frequencies, Pid),
            loop(NewFrequencies);
        {request, From, stop} ->
            reply(From, ok)
    end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    link(Pid),
    {{Free, [{Freq, Pid} | Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    case lists:keyfind(Freq, 1, Allocated) of
        {FreeFreq, Pid} ->
            unlink(Pid),
            NewAllocated = lists:keydelete(FreeFreq, 1, Allocated),
            {{[FreeFreq|Free], NewAllocated}, ok};
        _ -> {{Free, Allocated}, {error, unknown_frequency}}
    end.

handle_exit({Free, Allocated}, Pid) ->
    % Find all allocations for this Pid
    FreqsFreed = [Freq || {Freq, AllocPid} <- Allocated, AllocPid =:= Pid],
    case FreqsFreed of
        [] ->
            {Free, Allocated};
        _ ->
            % Remove all allocations for this Pid
            NewAllocated = [Alloc || Alloc <- Allocated, element(2, Alloc) =/= Pid],
            % Add all freed frequencies to Free
            {FreqsFreed ++ Free, NewAllocated}
    end.

%% Helper functions
get_frequencies() ->
    [10,11,12,13,14,15].
