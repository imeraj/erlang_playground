-module(processes).
-export([run/1]).

run(N) ->
    Loop = fun (0, _) -> ok; (Count, F) -> F(Count-1, F) end,
    BusyFun = fun() -> spawn(fun() -> Loop(1000000, Loop) end) end,
    SpawnThem = fun(Count) -> [BusyFun()  || _ <- lists:seq(1,Count)] end,
    GetStatus = fun() -> lists:sort([{erlang:process_info(P, [status]), P}
        || P <- erlang:processes()]) end,
    RunThem = fun(Count) -> SpawnThem(Count), GetStatus() end,
    RunThem(N).
