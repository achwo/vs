-module(sync_manager).
-export([start/2]).
-import(log, [log/4, debug/4]).

-define(DEVIATION_THRESHOLD_MS, 1).

-record(s, {
  offset,
  deviations = [],
  log
}).

start(TimeOffset, Log) -> 
  % log(Log, "Initializing...", []),
  State = #s{
    offset = TimeOffset,
    log = Log
  },
  spawn(fun() -> loop(State) end).

loop(State) ->
  receive 
    {add_deviation, StationType, SendTime, ReceiveTime} ->
      Deviation = SendTime - ReceiveTime,
      loop(addDeviation(State, StationType, Deviation));
    {reset_deviations} ->
      loop(resetDeviations(State));
    {From, get_current_time} ->
      loop(getCurrentTime(State, From));
    {sync} ->
      loop(sync(State));
    Any ->
      debug(State#s.log, "Received unknown message type: ~p", [Any]),
      loop(State)
  end.

addDeviation(State, StationType, Deviation)
  when StationType == "A" , abs(Deviation) > ?DEVIATION_THRESHOLD_MS ->
  debug(State#s.log, "addDeviation, StationType: A", []),
  State#s{ deviations = [Deviation | State#s.deviations] };
addDeviation(State, StationType, _) ->
  debug(State#s.log, "addDeviation, StationType: ~p", [StationType]),
  State.

resetDeviations(State) ->
  debug(State#s.log, "resetDeviations", []),
  State#s{ deviations = [] }.

getCurrentTime(State, From) ->
  CurrentTime = currentTime(State#s.offset),
  From ! {current_time, CurrentTime},
  State.

currentTime(TimeOffset) ->
  {MegaSecs, Secs, MicroSecs} = erlang:now(),
  (MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000) + TimeOffset.

sync(State) when length(State#s.deviations) == 0 ->
  State;
sync(State) ->
  NewOffset = State#s.offset + calculateNewOffset(State#s.deviations), 
  debug(State#s.log, "new Offset= ~p", [NewOffset]),
  State#s { offset = NewOffset }.

calculateNewOffset(Deviations) ->
  Sum = lists:sum(Deviations),
  Sum div length(Deviations).

% log(Log, Msg, Args) ->
%   {_, {Module, _Function, _Arity}} = process_info(self(), current_function),
%   log(Log, Module, Msg, Args).

debug(Log, Msg, Args) ->
  {_, {Module, _Function, _Arity}} = process_info(self(), current_function),
  debug(Log, Module, Msg, Args).