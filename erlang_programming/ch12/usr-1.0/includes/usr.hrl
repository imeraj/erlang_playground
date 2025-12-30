%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

%%% File    : usr.hrl
%%% Description : Include file for  user db

-type(plan()    :: prepay | postpay).
-type(status()  :: enabled | disabled).
-type(service() :: atom()).

-record(usr, {
    msisdn              :: integer(),
    id                  :: integer(),
    status = enabled    :: status(),
    plan                :: plan(),
    services = []       :: [service()]
}).
