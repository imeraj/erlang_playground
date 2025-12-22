-module(usr).
-export([start_link/0, start_link/1, stop/0]).
-export([init/1, terminate/2, handle_cast/2, handle_call/3]).
-export([add_usr/3, lookup_id/1, delete_usr/1, set_service/3, set_status/2, delete_disabled/0]).
-behaviour(gen_server).

-include("usr.hrl").

%% Exported client functions
%% Operation & Maintenance API

start_link() -> start_link("usrDb").

start_link(FileName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, FileName, []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% Customer Services API

add_usr(PhoneNum, CustId, Plan) when Plan==prepay; Plan==postpay ->
    gen_server:call(?MODULE, {add_usr, PhoneNum, CustId, Plan}).

lookup_id(CustId) ->
    gen_server:call(?MODULE, {lookup_id, CustId}).

delete_usr(CustId) ->
    gen_server:call(?MODULE, {delete_usr, CustId}).

set_service(CustId, Service, Flag) when Flag==true; Flag==false ->
    gen_server:call(?MODULE, {set_service, CustId, Service, Flag}).

set_status(CustId, Status) when Status==enabled; Status==disabled ->
 gen_server:call(?MODULE, {set_status, CustId, Status}).

delete_disabled() ->
  gen_server:call(?MODULE, delete_disabled).


%% Callback functions

init(FileName) ->
    usr_db:create_tables(FileName),
    usr_db:restore_backup(),
    {ok, null}.

handle_call({add_usr, PhoneNum, CustId, Plan}, _From, LoopData) ->
    Usr = #usr{msisdn = PhoneNum, id = CustId, plan = Plan},
    Reply = usr_db:add_usr(Usr),
    {reply, Reply, LoopData};

handle_call({lookup_id, CustId}, _from, LoopData) ->
    Reply = usr_db:lookup_id(CustId),
    {reply, Reply, LoopData};

handle_call({delete_usr, CustId}, _from, LoopData) ->
    Reply = usr_db:delete_usr(CustId),
    {reply, Reply, LoopData};

handle_call({set_service, CustId, Service, Flag}, _From, LoopData) ->
    Reply = case usr_db:lookup_id(CustId) of
        {ok, Usr} ->
            Services = lists:delete(Service, Usr#usr.services),
            NewServices = case Flag of
                true    -> [Service|Services];
                false   -> Services
            end,
            usr_db:update_usr(Usr#usr{services=NewServices});
        {error, instance} ->
            {error, instance}
    end,
    {reply, Reply, LoopData};

handle_call({set_status, CustId, Status}, _From, LoopData) ->
     Reply = case usr_db:lookup_id(CustId) of
        {ok, Usr} ->
            usr_db:update_usr(Usr#usr{status=Status});
        {error, instance} ->
            {error, instance}
        end,
        {reply, Reply, LoopData};

handle_call(delete_disabled, _From, LoopData) ->
    {reply, usr_db:delete_disabled(), LoopData}.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

terminate(_Reason, _LoopData) ->
    usr_db:close_tables().
