%%%===============================================================
%%% @author Meraj <meraj.enigma@gmail.com>
%%% @copyright Copyright (c)
%%% @doc RPC over TCP server.
%%% @end
%%%===============================================================

-module(tr_server).
-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0, start_link/1, get_count/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, request_count = 0}).

%%%===============================================================
%%% API
%%%===============================================================


%%----------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%----------------------------------------------------------------
-spec start_link(Port::integer) -> {ok, Pid} when Pid :: pid().
start_link(Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% @doc Calls start_link(Port) using default port.
-spec start_link() -> {ok, Pid} when Pid :: pid().
start_link() ->
  start_link(?DEFAULT_PORT).


%%----------------------------------------------------------------
%% @doc Fetches the number of requests made to the server.
%% @end
%%----------------------------------------------------------------
-spec get_count() -> {ok, Count} when Count :: integer().
get_count() ->
  gen_server:call(?SERVER, get_count).

%%----------------------------------------------------------------
%% @doc Stops the server.
%% @end
%%----------------------------------------------------------------
-spec stop() -> ok.
stop() ->
  gen_server:cast(?SERVER, stop).


%%%===============================================================
%%% gen_server callbacks
%%%===============================================================

init([Port]) ->
  {ok, Lsock} = gen_tcp:listen(Port, [{active, true}]),
  {ok, #state{port=Port, lsock=Lsock}, 0}.

handle_call(get_count, _From, State) ->
  {reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
  do_rpc(Socket, RawData),
  RequestCount = State#state.request_count + 1,
  {noreply, State#state{request_count=RequestCount}};
handle_info(timeout, #state{lsock=LSock} = State) ->
  {ok, _Sock} = gen_tcp:accept(LSock),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_oldVsn, State, _Extra) ->
  {ok, State}.

%%%===============================================================
%%% Internal functions
%%%===============================================================
do_rpc(Socket, RawData) ->
  try
    {M, F, A} = split_out_mfa(RawData),
      Result = apply(M, F, A),
      gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
  catch
      _Class:Err  ->
          gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))
  end.

split_out_mfa(RawData) ->
  MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
  {match, [M, F, A]} =
    re:run(MFA,
      "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
      [{capture, [1,2,3], list}, ungreedy]),
  {list_to_atom(M), list_to_atom(F), args_to_terms(A)}.

args_to_terms(RawArgs) ->
  {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
  {ok, Args} = erl_parse:parse_term(Toks),
  Args.


%% test

start_test() ->
  {ok, _} = tr_server:start_link(1055).

