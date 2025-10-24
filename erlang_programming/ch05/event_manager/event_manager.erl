-module(event_manager).
-export([start/2, stop/1]).
-export([add_handler/3, delete_handler/2, get_data/2, send_event/2]).
-export([init/1]).

start(Name, Handlers) ->
    register(Name, spawn(?MODULE, init, [Handlers])),
    ok.

add_handler(Name, Handler, InitData) ->
    call(Name, {add_handler, Handler, InitData}).

delete_handler(Name, Handler) ->
    call(Name, {delete_handler, Handler}).

get_data(Name, Handler) ->
    call(Name, {get_data, Handler}).

send_event(Name, Event) ->
    call(Name, {send_event, Event}).

stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.

init(Handlers) ->
    loop(initialize(Handlers)).

initialize([]) -> [];
initialize([{Handler, InitData} | Rest]) ->
    [{Handler, Handler:init(InitData)} | initialize(Rest)].

handle_msg({add_handler, Handler, InitData}, State) ->
    NewState = [{Handler, Handler:init(InitData)} | State],
    {ok, NewState};

handle_msg({delete_handler, Handler}, State) ->
    case lists:keysearch(Handler, 1, State) of
        false                     -> {{error, instance}, State};
        {value, {_Handler, Data}} ->
            Reply = {Handler, Handler:terminate(Data)},
            NewState = lists:keydelete(Handler, 1, State),
            {Reply, NewState}
    end;

handle_msg({get_data, Handler}, State) ->
    case lists:keysearch(Handler, 1, State) of
        false                     -> {{error, instance}, State};
        {value, {_Handler, Data}} -> {{data, Data}, State}
    end;

handle_msg({send_event, Event}, State) ->
    {ok, event(Event, State)}.

loop(State) ->
    receive
        {request, From, Msg} ->
            {Reply, NewState} = handle_msg(Msg, State),
            reply(From, Reply),
            loop(NewState);
        {stop, From} ->
            reply(From, terminate(State))
    end.

terminate([]) -> [];
terminate([{Handler, Data} | Rest]) ->
    [{Handler, Handler:terminate(Data)} | terminate(Rest)].

call(Name, Msg) ->
     Name ! {request, self(), Msg},
     receive {reply, Reply} -> Reply end.

reply(To, Msg) ->
    To ! {reply, Msg}.

event(_Event, []) -> [];
event(Event, [{Handler, Data} | Rest]) ->
    [{Handler, Handler:handle_event(Event, Data)} | event(Event, Rest)].
