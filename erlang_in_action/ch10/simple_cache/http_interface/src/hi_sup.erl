%%%-------------------------------------------------------------------
%% @doc tcp_interface top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(hi_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Port) ->
  {ok, Pid} = supervisor:start_link({local,?SERVER},?MODULE, [Port]),
  start_child(),
  {ok, Pid}.

start_child() ->
  supervisor:start_child(?SERVER, []).

init([Port]) ->
  ChildSpecList = [child(hi_server, worker, [Port])],
  RestartStrategy = {simple_one_for_one, 4, 3600},
  {ok,{RestartStrategy, ChildSpecList}}.

child(Module, Type, Params) ->
  {Module, {Module, start_link, Params},
    permanent, 2000, Type, [Module]}.

