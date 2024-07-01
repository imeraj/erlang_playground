-module(gws_connection_sup).

-behaviour(supervisor).

%% API
-export([start_link/4, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% API functions

start_link(Callback, IP, Port, UserArgs) ->
  {ok, Pid} = supervisor:start_link(?MODULE, [Callback, IP, Port, UserArgs]),
  start_child(Pid),
  {ok, Pid}.

start_child(Server) ->
  supervisor:start_child(Server, []).

%$ Supervisor callbacks
init([Callback, IP, Port, UserArgs]) ->
  BasicSockOpts = [binary,
    {active, false},
    {packet, http_bin},
    {reuseaddr, true}],
  SockOpts = case IP of
               undefined -> BasicSockOpts;
               _         -> [{ip,IP} | BasicSockOpts]
             end,
  {ok, LSock} = gen_tcp:listen(Port, SockOpts),
  ChildSpecList = [child(gws_server, worker, [Callback, LSock, UserArgs])],
  RestartStrategy = {simple_one_for_one, 1000, 3600},
  {ok,{RestartStrategy, ChildSpecList}}.

child(Module, Type, Params) ->
  {Module, {Module, start_link, Params},
    temporary, brutal_kill, Type, [Module]}.
