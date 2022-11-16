-module(sc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    ok = ensure_contact(),
    sc_store:init(),
    case sc_sup:start_link() of
        {ok, Pid} ->
            sc_event_logger:add_handler(),
            {ok, Pid};
        Other ->
            {error, Other}
    end.

stop(_State) ->
    ok.

%% internal functions
ensure_contact() ->
  DefaultNodes = ['contact1@MacBook-Pro', 'contact2@MacBook-Pro'],
  case get_env(simple_cache, contact_nodes, DefaultNodes) of
    [] ->
      {error, no_contact_nodes};
    ContactNodes ->
      ensure_contact(ContactNodes)
  end.

ensure_contact(ContactNodes) ->
  Answering = [N || N <- ContactNodes, net_adm:ping(N) =:= pong],
  case Answering of
    [] ->
      {error, no_contact_nodes_reachable};
    _ ->
      DefaultTime = 6000,
      WaitTime = get_env(simple_cache, wait_time, DefaultTime),
      wait_for_nodes(length(Answering), WaitTime)
  end.

wait_for_nodes(MinNodes, WaitTime) ->
  Slices = 10,
  SliceTime = round(WaitTime/Slices),
  wait_for_nodes(MinNodes, SliceTime, Slices).

wait_for_nodes(MinNodes, SliceTime, Iterations) ->
  case length(nodes()) > MinNodes of
    true ->
      ok;
    false ->
      timer:sleep(SliceTime),
      wait_for_nodes(MinNodes, SliceTime, Iterations - 1)
 end.

get_env(AppName, Key, Default) ->
  case application:get_env(AppName, Key) of
    undefined   -> Default;
    {ok, Value} -> Value
  end.