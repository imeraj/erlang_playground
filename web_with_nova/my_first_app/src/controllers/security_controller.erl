-module(security_controller).
-export([
         do_security/1
        ]).

do_security(Req) ->
    case get_user(Req) of
        {ok, User} -> {true, #{user => User}};
        _ -> false
    end.

get_user(Req) ->
    {ok, #{name => "Meraj", role => "admin"}}.
    