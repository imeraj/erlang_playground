%%%-------------------------------------------------------------------
%% @doc tcp_interface top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ti_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(LSock) ->
  supervisor:start_link({local,?SERVER},?MODULE, [LSock]).

start_child() ->
  supervisor:start_child(?SERVER, []).

init([LSock]) ->
  ChildSpecList = [child(ti_server, worker, [LSock])],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok,{RestartStrategy, ChildSpecList}}.

child(Module, Type, Params) ->
  {Module, {Module, start_link, Params},
    temporary, brutal_kill, Type, [Module]}.

