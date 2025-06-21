-module(foo).
-export([hello_world/0]).

%% Simple hello world function
hello_world() ->
    io:format("Hello, World!~n").
