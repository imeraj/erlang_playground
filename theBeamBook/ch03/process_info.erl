-module(process_info).

-export([info/1, backtrace/1, heap_sort/0]).

info(Name) when is_atom(Name) ->
    Pid = whereis(Name),
    erlang:process_info(Pid).

backtrace(Name) when is_atom(Name) ->
    Pid = whereis(Name),
    [{backtrace, Backtrace}] = erlang:process_info(Pid, [backtrace]),
    Backtrace.

heap_sort() ->
    Processes =
        lists:keysort(2,
                      [{Pid,
                        proplists:get_value(total_heap_size,
                                            erlang:process_info(Pid, [total_heap_size]))}
                       || Pid <- erlang:processes()]),
    lists:reverse(Processes).
