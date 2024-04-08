-module(hlr).
-export([new/0, attach/2, detach/1, lookup_id/1, lookup_ms/1]).

new() ->
  ets:new(msisdn2pid, [public, named_table]),
  ets:new(pid2msisdn, [public, named_table]),
  ok.

attach(Ms, Pid) ->
  ets:insert(msisdn2pid, {Ms, Pid}),
  ets:insert(pid2msisdn, {Pid, Ms}).

detach(Pid) ->
  case ets:lookup(pid2msisdn, Pid) of
    [{Pid, Ms}] ->
      ets:delete(msisdn2pid, Ms),
      ets:delete(pid2msisdn, Pid);

    [] ->
      ok
  end.

lookup_id(Ms) ->
  case ets:lookup(msisdn2pid, Ms) of
    [{Ms, Pid}] -> {ok, Pid};

    [] ->
      {error, invalid}
  end.

lookup_ms(Pid) ->
  case ets:lookup(pid2msisdn, Pid) of
    [{Pid, Ms}] -> {ok, Ms};

    [] ->
      {error, invalid}
  end.