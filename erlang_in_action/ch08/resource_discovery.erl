-module(resource_discovery).
-behaviour(gen_server).

-export([
  start_link/0,
  add_target_resource_types/1,
  add_local_resource/2,
  fetch_resources/1,
  trade_resources/0
]).

-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).
-record(state, {target_resource_types,
                local_resource_tuples,
                found_resource_tuples}).

%%API
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_target_resource_types(Type) ->
  gen_server:cast(?SERVER, {add_target_resource_types, Type}).

add_local_resource(Type, Instance) ->
  gen_server:cast(?SERVER, {add_local_resource, {Type, Instance}}).

fetch_resources(Type) ->
  gen_server:call(?SERVER, {fetch_resources, Type}).

trade_resources() ->
  gen_server:cast(?SERVER, trade_resources).

%%Callback
init([]) ->
  {ok, #state{target_resource_types = [],
                local_resource_tuples = dict:new(),
                found_resource_tuples = dict:new()}}.

handle_call({fetch_resources, Type}, _From, State) ->
  {reply, dict:find(Type, State#state.found_resource_tuples), State}.

handle_cast({add_target_resource_types, Type}, State) ->
  TargetTypes = State#state.target_resource_types,
  NewTargetTypes = [Type | lists:delete(Type, TargetTypes)],
  {noreply, State#state{target_resource_types = NewTargetTypes}};
handle_cast({add_local_resource, {Type, Instance}}, State) ->
  ResourceTuples = State#state.local_resource_tuples,
  NewResourceTuples = add_resource(Type, Instance, ResourceTuples),
  {noreply, State#state{local_resource_tuples = NewResourceTuples}};
handle_cast(trade_resources, State) ->
  ResourceTuples = State#state.local_resource_tuples,
  AllNodes = [node() | nodes()],
  lists:foreach(
    fun(Node) ->
      gen_server:cast({?SERVER, Node}, {trade_resources, {node(), ResourceTuples}})
    end,
    AllNodes),
  {noreply, State};
handle_cast({trade_resources, {ReplyTo, Remotes}}, State) ->
  #state{local_resource_tuples = Locals,
          target_resource_types = TargetTypes,
          found_resource_tuples = OldFound} = State,
  FilteredRemotes = resource_for_types(TargetTypes, Remotes),
  NewFound = add_resources(FilteredRemotes, OldFound),
  case ReplyTo of
    noreply ->
      ok;
    _ ->
      gen_server:cast({?SERVER, ReplyTo}, {trade_resources, {noreply, Locals}})
  end,
  {noreply, State#state{found_resource_tuples = NewFound}}.

%%Internal functions
add_resource(Type, Resource, ResourceTuples) ->
  case dict:find(Type, ResourceTuples) of
    {ok, ResourceList} ->
      NewList = [Resource | lists:delete(Resource, ResourceList)],
      dict:store(Type, NewList, ResourceTuples);
    error ->
      dict:store(Type, [Resource], ResourceTuples)
  end.

resource_for_types(Types, ResourceTuples) ->
  Fun =
    fun(Type, Acc) ->
      case dict:find(Type, ResourceTuples) of
        {ok, ResourceList} ->
          [{Type, Instance} || Instance <- ResourceList] ++ Acc;
        error ->
          Acc
      end
    end,
  lists:foldl(Fun, [], Types).

add_resources([{Type, Resource} | T], ResourceTuples) ->
  add_resources(T, add_resource(Type, Resource, ResourceTuples));
add_resources([], ResourceTuples) ->
  ResourceTuples.
