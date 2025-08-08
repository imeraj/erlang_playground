-module(personaldb_router).

-behaviour(nova_router).

-export([routes/1]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [#{prefix => "/users",
       security => false,
       routes =>
           [{"/", fun personaldb_main_controller:add/1, #{methods => [post]}},
            {"/", fun personaldb_main_controller:index/1, #{methods => [get]}},
            {"/:id", fun personaldb_main_controller:get_by_id/1, #{methods => [get]}},
            {"/:id", fun personaldb_main_controller:update/1, #{methods => [patch]}},
            {"/:id", fun personaldb_main_controller:delete/1, #{methods => [delete]}},
            {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}]}].
