-module(sync_manager).
-export([start/2]).
-import(log, [log/4, debug/4]).

start(TimeOffset, Log) -> 
  log(Log, "Initializing...", []),
  spawn(fun() -> loop(TimeOffset, [], Log) end).

loop(TimeOffset, Deviations, Log) ->
  receive 
    {add_deviation, StationType, SendTime, ReceiveTime} ->
      NewDeviations = addDeviation(StationType, SendTime, ReceiveTime, Deviations),
      loop(TimeOffset, NewDeviations, Log);
    {reset_deviations} ->
      NewDeviations = resetDeviations(),
      loop(TimeOffset, NewDeviations, Log);
    {From, get_current_time} ->
      getCurrentTime(From, TimeOffset),
      loop(TimeOffset, Deviations, Log);
    {sync} ->
      NewTimeOffset = sync(TimeOffset, Deviations),
      loop(NewTimeOffset, Deviations, Log);
    Any ->
      debug(Log, "SyncManager: Received unknown message type: ~p", [Any]),
      loop(TimeOffset, Deviations, Log)
  end.

addDeviation(StationType, SendTime, ReceiveTime, Deviations) 
  when StationType == "A" ->
  Deviation = SendTime - ReceiveTime,
  [Deviation | Deviations];
addDeviation(_, _, _, Deviations) ->
  Deviations.

resetDeviations() ->
  [].

getCurrentTime(From, TimeOffset) ->
  CurrentTime = currentTime(TimeOffset),
  From ! {current_time, CurrentTime}.

currentTime(TimeOffset) ->
  {MegaSecs, Secs, MicroSecs} = erlang:now(),
  (MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000) + TimeOffset.

sync(TimeOffset, Deviations) when length(Deviations) == 0 ->
  TimeOffset;
sync(TimeOffset, Deviations) ->
  TimeOffset + calculateNewOffset(Deviations).

calculateNewOffset(Deviations) ->
  Sum = lists:sum(Deviations),
  Sum div length(Deviations). %Special case if own station is class A?

log(Log, Msg, Args) ->
  {_, {Module, _Function, _Arity}} = process_info(self(), current_function),
  log(Log, Module, Msg, Args).

debug(Log, Msg, Args) ->
  {_, {Module, _Function, _Arity}} = process_info(self(), current_function),
  debug(Log, Module, Msg, Args).