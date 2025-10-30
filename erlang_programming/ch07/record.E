-file("record.erl", 1).

-module(record).

-export([birthday/1,joe/0,showPerson/1]).

-record(name,{first, last}).

-record(person,{name = #name{}, age = 0, phone}).

birthday({person, _, Age, _} = P) ->
    begin
        REC0 = Age + 1,
        REC1 = P,
        case REC1 of
            {person, _, _, _} ->
                setelement(3, REC1, REC0);
            _ ->
                error({badrecord, REC1})
        end
    end.

joe() ->
    {person, {name, "joe", "armstrong"}, 21, "999-999"}.

showPerson({person, {name, FirstName, LastName}, Age, Phone}) ->
    io:format("name: [~p ~p], age: ~p, phone: ~p~n",
              [FirstName, LastName, Age, Phone]).



