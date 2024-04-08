-module(addr).
-export([type/1, hostnet/2]).

-include_lib("kernel/include/inet.hrl").

type(Addr) ->
  {ok, HostEnt} = inet:gethostbyaddr(Addr),
  HostEnt#hostent.h_addrtype.

hostnet(Host, inet) ->
  #hostent{h_name=Host, h_addrtype=inet, h_length=4,
    h_addr_list=inet:getaddrs(Host, inet)}.
