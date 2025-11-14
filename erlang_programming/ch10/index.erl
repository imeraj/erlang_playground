-module(index).
-export([index/1]).

-define(Punctuation,"(\\ |\\,|\\.|\\;|\\:|\\t|\\n|\\(|\\))+").

index(File) ->
    ets:new(indexTable, [ordered_set, named_table]),
    processFile(File),
    prettyIndex().

processFile(File) ->
    {ok, Fd} = file:open(File, [read]),
    processLines(Fd, 1).

processLines(Fd, N) ->
    case io:get_line(Fd, "") of
        eof ->
            ok;
        Line ->
            processLine(Line, N),
            processLines(Fd, N+1)
    end.

processLine(Line, N) ->
    case re:split(Line, ?Punctuation) of
        [] -> [];
        Words ->
            processWords(Words, N)
    end.

processWords(Words, N) ->
    case Words of
        [] -> ok;
        [Word|Rest] ->
            WordList = binary_to_list(Word),
            if
                length(WordList) >= 3 ->
                    Normalize = string:to_lower(WordList),
                    ets:insert(indexTable, {Normalize, N});
                true ->
                    ok
            end,
            processWords(Rest, N)
    end.

prettyIndex() ->
    case ets:first_lookup(indexTable) of
        '$end_of_table' ->
            ok;
        First ->
            case First of
                {FirstKey, [{Word, N}]} ->
                    IndexEntry = {Word, [N]}
            end,
            prettyIndexNext(FirstKey, IndexEntry)
    end.

prettyIndexNext(Key, {Word, Lines} = IndexEntry) ->
    Next = ets:next_lookup(indexTable, Key),
    case Next of
        '$end_of_table' ->
            prettyIndex(IndexEntry);
        {NextKey, [{NextWord, M}]} ->
            if
                NextWord == Word ->
                    prettyIndexNext(NextKey, {Word, [M|Lines]});
                true ->
                    prettyIndex(IndexEntry),
                    prettyIndexNext(NextKey, {NextWord, [M]})
            end
    end.

 prettyIndex({Word, Lines}) ->
    io:format("~p appears on lines: ", [Word]),
    lists:foreach(fun(H) -> io:format("~p ", [H]) end, Lines),
    io:format("~n").
