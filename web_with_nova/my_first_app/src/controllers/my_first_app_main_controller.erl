-module(my_first_app_main_controller).
-export([
         index/1,
         error_example/1
        ]).
-fallback_controller(my_fallback_controller).

index(Req = #{auth_data := User}) ->
    io:format("User: ~p~n", [User]),
    {ok, [{message, "Hello world!"}]}.

error_example(_Req) ->
    {error, example_error}.
