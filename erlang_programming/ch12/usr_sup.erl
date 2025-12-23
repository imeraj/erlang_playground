-module(usr_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    UsrChild = {usr, {usr, start_link, []},
                    permanent, 2000, worker, [usr, usr_db]},
    {ok, {{one_for_one, 1, 1}, [UsrChild]}}.
