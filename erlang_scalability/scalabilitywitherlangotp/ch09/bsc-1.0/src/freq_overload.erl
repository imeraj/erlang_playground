-module(freq_overload).
-export([start_link/0, add/2, delete/2]).
-export([no_frequency/0, frequency_available/0, frequency_denied/0]).

start_link() ->
  case gen_event:start_link({global, ?MODULE}) of
    {ok, Pid} ->
      add(counters, {}),
      add(logger, {file, "log"}),
      {ok, Pid};
    Error ->
      Error
  end.

no_frequency() -> gen_event:notify({global, ?MODULE}, {set_alarm, {no_frequency, self()}}).
frequency_available() -> gen_event:notify({global, ?MODULE}, {clear_alarm, no_frequency}).
frequency_denied() -> gen_event:notify({global, ?MODULE}, {event, {frequency_denied, self()}}).

add(M,A) -> gen_event:add_sup_handler({global, ?MODULE}, M, A).
delete(M,A) -> gen_event:delete_handler({global, ?MODULE},M,A).