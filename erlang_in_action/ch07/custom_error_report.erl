-module(custom_error_report).
-behaviour(gen_event).

-export([register_with_logger/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%%API
register_with_logger() ->
  error_logger:add_report_handler(?MODULE).

%%Callbacks
init([]) ->
  {ok, #state{}}.

handle_event({error, _Gleader, {Pid,Format,Data}}, State) ->
  io:fwrite("ERROR <~p> ~s", [Pid, io_lib:format(Format, Data)]),
  {ok, State};
handle_event({error_report, _Gleader, {Pid, std_error, Report}}, State) ->
  io:fwrite("ERROR <~p> ~p", [Pid, Report]),
  {ok, State};
handle_event({error_report, _Gleader, {Pid, Type, Report}}, State) ->
  io:fwrite("ERROR <~p> ~p ~p", [Pid, Type, Report]),
  {ok, State};
handle_event({warning_msg, _Gleader, {Pid, Format, Data}}, State) ->
  io:fwrite("WARNING <~p> ~s", [Pid, io_lib:format(Format, Data)]),
  {ok, State};
handle_event({warning_report,_Gleader,{Pid,std_warning,Report}}, State) ->
  io:fwrite("WARNING <~p> ~p", [Pid, Report]),
  {ok, State};
handle_event({warning_report,_Gleader,{Pid, Type, Report}}, State) ->
  io:fwrite("WARNING <~p> ~p ~p", [Pid, Type, Report]),
  {ok, State};
handle_event({info_msg, _Gleader, {Pid, Format, Data}}, State) ->
  io:fwrite("INFO <~p> ~s", [Pid, io_lib:format(Format, Data)]),
  {ok, State};
handle_event({info_report, _Gleader, {Pid, std_info, Report}}, State) ->
  io:fwrite("INFO <~p> ~p", [Pid, Report]),
  {ok, State};
handle_event({info_report, _Gleader, {Pid, Type, Report}}, State) ->
  io:fwrite("INFO <~p> ~p ~p", [Pid, Type, Report]),
  {ok, State};
handle_event(_Event, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  {reply, ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.