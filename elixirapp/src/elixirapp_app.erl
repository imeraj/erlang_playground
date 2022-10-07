%%%-------------------------------------------------------------------
%% @doc elixirapp public API
%% @end
%%%-------------------------------------------------------------------

-module(elixirapp_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([to_decimal/1, last/1, downcase/1]).

start(_StartType, _StartArgs) ->
    elixirapp_sup:start_link().

stop(_State) ->
    ok.

to_decimal(Number) ->
    Val = 'Elixir.Decimal':from_float(Number/1),
    io:format("~s~n", ['Elixir.Decimal':to_string(Val)]).

last(List) when is_list(List) ->
    io:format("~p~n", ['Elixir.List':last(List)]).

downcase(Bin) when is_binary(Bin) ->~~
    io:format("~s~n", ['Elixir.String':downcase(Bin)]).


%% internal functions
