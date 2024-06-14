-module(tr_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([stop/0]).

start_link() ->
  supervisor:start_link({local,?MODULE}, ?MODULE, []).

stop() -> exit(global:whereis_name({local,?MODULE}), shutdown).

init(_) ->
  ChildSpecList = [child(tr_server, worker)],
  {ok,{{one_for_one, 5, 3600}, ChildSpecList}}.

child(Module, Type) ->
  {Module, {Module, start_link, []},
    permanent, 2000, Type, [Module]}.
