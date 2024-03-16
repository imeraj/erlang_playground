-module(tut).

-export([convert/2, format_temps/1, foreach/2, map/2, convert_list_to_c/1]).

convert(inch, M) ->
    {inch, M / 2.54};
convert(cm, M) ->
    {cm, M * 2.54}.

format_temps([]) ->
    ok;
format_temps([City | Rest]) ->
    print_temp(convert_to_celsius(City)),
    format_temps(Rest).

convert_to_celsius({Name, {c, Temp}}) ->
    {Name, {c, Temp}};
convert_to_celsius({Name, {f, Temp}}) ->
    {Name, {c, (Temp - 32) * 5 / 9}}.

print_temp({Name, {c, Temp}}) ->
    io:format("~-15w: ~w c~n", [Name, Temp]).

foreach(Fun, [First | Rest]) ->
    Fun(First),
    foreach(Fun, Rest);
foreach(_Fun, []) ->
    ok.

map(Fun, [First | Rest]) ->
    [Fun(First) | map(Fun, Rest)];
map(_Fun, []) ->
    [].

convert_list_to_c(List) ->
    New_List = lists:map(fun convert_to_c/1, List),
    lists:sort(fun({_, {_, Temp1}}, {_, {_, Temp2}}) -> Temp1 < Temp2 end, New_List).

convert_to_c ( { Name , { c , Temp } } ) -> { Name , { c , Temp } };
convert_to_c ( { Name , { f , Temp } } ) -> { Name , { c , ( Temp - 32 ) * 5 / 9 } }.
