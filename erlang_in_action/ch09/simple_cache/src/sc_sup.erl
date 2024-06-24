-module(sc_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local,?SERVER}, ?MODULE, []).

init(_) ->
  ChildSpecList = [child(sc_element_sup, supervisor), child(sc_event, worker)],
  RestartStrategy = {one_for_one, 4, 3600},
  {ok,{RestartStrategy, ChildSpecList}}.

child(Module, Type) ->
  {Module, {Module, start_link, []},
    permanent, 2000, Type, [Module]}.
