-module(my_module).
-export([pie/0, sign/1, yesorno/2, create_customer/3, print_customer/1, update_customer/2]).

-record(customer, {name="anonymouse", address, phone}).

pie() ->
  3.14.

sign(N) when is_number(N) ->
  case N < 0 of
    true -> negative;
    false -> positive
  end.

yesorno(F, N) when is_number(N) ->
  F(N).

create_customer(Name, Address, Phone) ->
  #customer{name=Name, address = Address, phone = Phone}.

update_customer(R, Name) ->
  R#customer{name=Name}.

print_customer(#customer{name=Name, address = Address, phone = Phone}) ->
  io:format("Name: ~p\nAddress: ~p\nPhone: ~p\n", [Name, Address, Phone]).