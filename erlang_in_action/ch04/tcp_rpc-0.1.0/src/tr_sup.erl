-module(tr_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({global,?MODULE}, ?MODULE, []).

init(_) ->
  ChildSpecList = [child(tr_server, worker)],
  {ok,{{one_for_one, 5, 3600}, ChildSpecList}}.

child(Module, Type) ->
  {Module, {Module, start_link, []},
    permanent, 2000, Type, [Module]}.
