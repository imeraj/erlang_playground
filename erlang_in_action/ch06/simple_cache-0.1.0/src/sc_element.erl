-module(sc_element).
-behaviour(gen_server).

-export([
  start_link/2,
  create/2,
  create/1,
  fetch/1,
  replace/2,
  delete/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER,  ?MODULE).
-define (DEFAULT_LEASE_TIME, (60*60*24)).

-record(state, {value, lease_time, start_time}).

%%APIs
start_link(Value, LeaseTime) ->
  gen_server:start_link(?SERVER, {Value, LeaseTime}, []).

create(Value, LeaseTime) ->
  sc_element_sup:start_child(Value, LeaseTime).

create(Value) ->
  create(Value, ?DEFAULT_LEASE_TIME).

fetch(Pid) ->
  gen_server:call(Pid, fetch).

replace(Pid, Value) ->
  gen_server:cast(Pid, {replace, Value}).

delete(Pid) ->
  gen_server:cast(Pid, delete).

%%Callbacks
init({Value, LeaseTime}) ->
  Now = calendar:local_time(),
  StartTime = calendar:datetime_to_gregorian_seconds(Now),
  TimeLeft = time_left(StartTime, LeaseTime),
  {ok, #state{value = Value, lease_time = LeaseTime, start_time = StartTime}, TimeLeft}.

handle_call(fetch, _From, State) ->
  #state{value = Value, lease_time = LeaseTime, start_time = StartTime} = State,
  TimeLeft = time_left(StartTime, LeaseTime),
  {reply, {ok, Value}, State, TimeLeft}.

handle_cast({replace, Value}, State) ->
  #state{lease_time = LeaseTime, start_time = StartTime} = State,
  TimeLeft = time_left(StartTime, LeaseTime),
  {noreply, State#state{value = Value}, TimeLeft};
handle_cast(delete, State) ->
  {stop, normal, State}.

handle_info(timeout, State) ->
  {stop, normal, State}.

terminate(_Reason, _State) ->
  sc_store:delete(self()),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%internal functions
time_left(_StartTime, infinity) ->
  infinity;
time_left(StartTime, LeaseTime) ->
  Now = calendar:local_time(),
  CurrentTime = calendar:datetime_to_gregorian_seconds(Now),
  ElapsedTime = CurrentTime - StartTime,
  case LeaseTime - ElapsedTime of
    Time when Time =< 0 -> 0;
    Time -> Time * 1000
  end.



