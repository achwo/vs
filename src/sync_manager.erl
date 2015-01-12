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
      resetDeviations(),
      loop(StationType, TimeOffset, Deviations);
    {From, get_current_time} ->
      getCurrentTime(From),
      loop(StationType, TimeOffset, Deviations)
  end.

addDeviation(StationType, SendTime, ReceiveTime, Deviations) 
  when StationType == "A" ->
  Deviation = SendTime - ReceiveTime,
  [Deviation | Deviations];
addDeviation(_, _, _, Deviations) ->
  Deviations.

resetDeviations() ->
  % clear deviation list
  todo.

getCurrentTime(From) ->
  % From ! {current_time, CurrentTime}.
  todo.