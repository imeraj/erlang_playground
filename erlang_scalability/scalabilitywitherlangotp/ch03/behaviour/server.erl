-module(server).
-export([start/2, stop/1, call/2]).
-export([init/2]).

start(Name, Args) ->
  register(Name, spawn(server, init, [Name, Args])).

stop(Name) ->
  Name ! {stop, self()},
  receive
    {reply, Reply} -> Reply
  end.

init(Mod, Args) ->
  State = Mod:init(Args),
  loop(Mod, State).

%% Important details in the book
call(Name, Msg) ->
  Ref = erlang:monitor(process, Name),
  catch Name ! {request, {Ref, self()}, Msg},
  receive
    {reply, Ref, Reply} ->
      erlang:demonitor(Ref, [flush]),
      Reply;
    {'DOWN', Ref, process, _Name, _Reason} ->
      {error, no_proc}
  end.

loop(Mod, State) ->
  receive
    {request, From, Msg} ->
      {NewState, Reply} = Mod:handle(Msg, State),
      reply(From, Reply),
      loop(Mod, NewState);
    {stop, From} ->
      Reply = Mod:terminate(State),
      reply(From, Reply)
  end.

reply({Ref, To}, Reply) ->
  To ! {reply, Ref, Reply}.