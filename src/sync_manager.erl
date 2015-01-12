-module(sync_manager).
-export([start/2]).

start(StationType, TimeOffset) -> 
  spawn(fun() -> loop(StationType, TimeOffset, []) end).

loop(StationType, TimeOffset, Deviations) ->
  receive 
    {add_deviation, StationType, SendTime, ReceiveTime} ->
      NewDeviations = addDeviation(StationType, SendTime, ReceiveTime, Deviations),
      loop(StationType, TimeOffset, NewDeviations);
    {reset_deviations} ->
      NewDeviations = resetDeviations(),
      loop(StationType, TimeOffset, NewDeviations);
    {From, get_current_time} ->
      getCurrentTime(From, TimeOffset),
      loop(StationType, TimeOffset, Deviations)
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