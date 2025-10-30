-module(record).
-export([birthday/1, joe/0, showPerson/1]).

-record(name, {first, last}).
-record(person, {name=#name{}, age = 0, phone}).

birthday(#person{age=Age} = P) ->
    P#person{age=Age+1}.

joe() ->
    #person{name=#name{first="joe",last="armstrong"}, age=21, phone="999-999"}.

showPerson(#person{name=#name{first=FirstName,last=LastName}, age=Age, phone=Phone}) ->
    io:format("name: [~p ~p], age: ~p, phone: ~p~n", [FirstName, LastName, Age, Phone]).
