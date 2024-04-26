-module(frequency_nonotp_sup).
-behaviour(supervisor_bridge).

-export([start_link/0, stop/0]).
-export([init/1, terminate/2]).

%% For some reason supervisor_bridge not restarting child

start_link() ->
  supervisor_bridge:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
  exit(whereis(?MODULE), shutdown).

init([]) ->
  frequency_nonotp:start(),
  case whereis(?MODULE) of
    Pid when is_pid(Pid) ->
      {ok, Pid, []};
    undefined ->
      {error, start_error}
  end.

terminate(_Reason, []) ->
  frequency_nonotp:stop().
