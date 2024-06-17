-module(sc_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/2]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local,?SERVER}, ?MODULE, []).

start_child(Value, LeaseTime) ->
  supervisor:start_child(?SERVER, [Value, LeaseTime]).

init(_) ->
  ChildSpecList = [child(sc_element, worker)],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok,{RestartStrategy, ChildSpecList}}.

child(Module, Type) ->
  {Module, {Module, start_link, []},
    temporary, brutal_kill, Type, [Module]}.
