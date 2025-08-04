-module(my_first_app_main_controller).
-export([
         index/1
        ]).

index(Req = #{auth_data := User}) ->
    io:format("User: ~p~n", [User]),
    {ok, [{message, "Hello world!"}]}.
