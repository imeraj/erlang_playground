-module(tcp).
-export([client/2]).
-export([server/0, get_request/2]).

client(Host, Data) ->
    {ok, Socket} = gen_tcp:connect(Host, 1234, [binary, {packet, 0}]),
    gen_tcp:send(Socket, Data),
    ok = gen_tcp:close(Socket).

server() ->
    {ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, false}]),
    wait_connect(ListenSocket, 0).

wait_connect(ListenSocket, Count) ->
    {ok, ClientSocket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE, get_request, [ClientSocket, Count]),
    wait_connect(ListenSocket, Count + 1).

get_request(Socket, Count) ->
    get_request(Socket, [], Count).

get_request(Socket, BinaryList, Count) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Binary} ->
            get_request(Socket, [Binary | BinaryList], Count);
        {error, closed} ->
            handle(lists:reverse(BinaryList), Count)
    end.

handle(BinaryList, Count) ->
    {ok, Fd} = file:open("log_file_" ++ integer_to_list(Count), [write]),
    ok = file:write(Fd, iolist_to_binary(BinaryList)),
    ok = file:close(Fd).
