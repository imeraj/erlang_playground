-module(perf_example).
-export([compute/1, eprof/0, tprof/1, fprof/0, cprof/0]).

compute([]) ->
    ok;
compute([H|T]) ->
    _F = factorial(H),
    compute(T).

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N-1).

eprof() ->
    Compute = fun() -> perf_example:compute([10,15,20,25,30]) end,
    eprof:profile(Compute),
    eprof:analyze().

tprof(Type) ->
    Compute = fun() -> perf_example:compute([10,15,20,25,30]) end,
    tprof:profile(Compute, #{type => Type}).

fprof() ->
    fprof:apply(fun perf_example:compute/1, [[10,15,20,25,30]]),
    fprof:profile(),
    fprof:analyse().

cprof() ->
    cprof:start(perf_example),
    perf_example:compute([10,15,20,25,30]),
    cprof:pause(),
    cprof:analyse().

