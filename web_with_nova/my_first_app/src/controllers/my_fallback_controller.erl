-module(my_fallback_controller).
-export([
    resolve/2
]).

resolve(Req, {error, example_error}) ->
    io:format("return from fallback controller..."),
    {status, 400}.