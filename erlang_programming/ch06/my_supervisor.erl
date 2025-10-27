-module(my_supervisor).
-export([start_link/2, stop/1]).
-export([init/1]).

start_link(Name, ChildSpecList) ->
    register(Name, spawn(?MODULE, init, [ChildSpecList])),
    ok.

stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.

init(ChildSpecList) ->
    process_flag(trap_exit, true),
    ChildList = start_children(ChildSpecList),
    loop(ChildList).

start_children([]) -> [];
start_children([{M,F,A}|ChildSpecList]) ->
   case (catch apply(M,F,A)) of
       {ok, Pid} ->
           link(Pid),
           [{Pid, {M,F,A}} | start_children(ChildSpecList)];
       _ -> start_children(ChildSpecList)
   end.

restart_child(Pid, ChildList) ->
    case lists:keyfind(Pid, 1, ChildList) of
        {OldPid, {M,F,A}} ->
            {ok, NewPid} = apply(M,F,A),
            [{NewPid, {M,F,A}} | lists:keydelete(OldPid, 1, ChildList)];
        _ -> ChildList
    end.

loop(ChildList) ->
    receive
        {'EXIT', Pid, Reason} ->
            io:format("~p terminated with Reason - ~p~n", [Pid, Reason]),
            restart_child(Pid, ChildList),
            loop(ChildList);
        {stop, From} ->
            From ! {reply, terminate(ChildList)}
    end.

terminate([{Pid, _}, ChildList]) ->
    exit(Pid, kill),
    terminate(ChildList);
terminate(_childList) -> ok.
