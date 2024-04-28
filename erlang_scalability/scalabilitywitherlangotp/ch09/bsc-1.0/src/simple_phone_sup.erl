-module(simple_phone_sup).
-behaviour(supervisor).

-export([start_link/0, attach_phone/1, detach_phone/1]).
-export([init/1]).

start_link() ->
  supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
  hlr:new(),

  {ok, {{simple_one_for_one, 10, 3600},
    [{ms, {phone_fsm, start_link, []},
      transient, 2000, worker, [phone_fsm]}]}}.

attach_phone(Ms) ->
  case hlr:lookup_id(Ms) of
    {ok, _Pid} ->
      {error, attached};
    _NotAttached ->
      supervisor:start_child({global, ?MODULE}, [Ms])
  end.

detach_phone(Ms) ->
  case hlr:lookup_id(Ms) of
    {ok, Pid} ->
      supervisor:terminate_child({global, ?MODULE}, Pid);
    _NotAttached ->
      {error, detached}
  end.