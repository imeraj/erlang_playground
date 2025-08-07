-module(personaldb_main_controller).
-export([
         add/1,
         index/1,
         get_by_id/1
        ]).

add(#{json := #{<<"age">> := Age,<<"id">> := Id}}) ->
    try
        {ok, Port} = eredis:start_link(),
        {ok, Results} = eredis:q(Port, ["hset", "users" | [Id, Age]]),
        eredis:stop(Port),
        {json, 201, #{}, #{<<"resp">> => Results}}
    catch
        Error:Cause ->
            {json,500,#{<<"Content-Type">> => <<"json">>},
            #{<<"error">> => Error, <<"cause">> => Cause}}
    end.

index(_Req) ->
    try
        {ok, Port} = eredis:start_link(),
        {ok, Results} = eredis:q(Port, ["hgetall", "users"]),
        Resp = flat_list_to_map(Results, []),
        eredis:stop(Port),
        {json, 200, #{}, #{<<"resp">> => Resp}}
   catch
       Error:Cause ->
           {json,500,#{<<"Content-Type">> => <<"json">>},
           #{<<"error">> => Error, <<"cause">> => Cause}}
   end.

 get_by_id(#{parsed_qs := #{<<"id">> := UserId}}) ->
       try
           {ok, Port} = eredis:start_link(),
           case eredis:q(Port, ["hget", "users", UserId]) of
               {ok, undefined} ->
                   eredis:stop(Port),
                   {status, 404};
               {ok, Results} ->
                   eredis:stop(Port),
                   {json, 200, #{}, #{<<"id">> => UserId, <<"age">> => Results}}
           end
       catch
          Error:Cause ->
              {json, 500, #{<<"Content-Type">> => <<"json">>},
              #{<<"error">> => Error, <<"cause">> => Cause}}
       end.

flat_list_to_map([], Acc) ->
    Acc;
flat_list_to_map([K, V | Rest], Acc) ->
    flat_list_to_map(Rest, [#{<<"id">> => K,  <<"age">> => V} | Acc]).
