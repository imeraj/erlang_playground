-module(my_first_app_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [#{prefix => "/admin",
       plugins => [{pre_request, nova_request_plugin, #{decode_json_body => true}}],
       security => fun security_controller:do_security/1,
       routes => [
                 {"/", fun my_first_app_main_controller:index/1 , #{methods => [get]}},
                 {"/error", fun my_first_app_main_controller:error_example/1 , #{methods => [get]}},
                 {"/with_favicon", "assets/favicon.ico", #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
      }].
