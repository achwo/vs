-module(sync_manager).
-export([start/1]).

-define(DEVIATION_THRESHOLD_MS, 1).

-record(s, {
  offset = 0,
  deviations = []
}).

start(Offset) ->
  spawn(fun() -> loop(#s{ offset = Offset }) end).

loop(State) ->
  receive
    {add_deviation, StationType, SendTime, ReceiveTime} ->
      loop(addDeviation(State, StationType, SendTime - ReceiveTime));
    {From, get_current_time} ->
      Time = currentTime(State#s.offset),
      From ! {current_time, Time},
      loop(State);
    {sync} ->
      loop(sync(State));
    {reset_deviations} ->
      loop(State#s{ deviations = [] });
    Any -> loop(unknown(State, Any))
  end.

addDeviation(State, ForeignType, Deviation)
when ForeignType == "A", abs(Deviation) > ?DEVIATION_THRESHOLD_MS ->
  State#s{ deviations =  [Deviation|State#s.deviations] };
addDeviation(State, _, _) ->
  State.

currentTime(Offset) ->
  {MegaSecs, Secs, MicroSecs} = erlang:now(),
  (MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000) + Offset.

sync(State) when length(State#s.deviations) == 0 ->
  State;
sync(State) ->
  Offset = calculateNewOffset(State#s.deviations),
  State#s{ offset = State#s.offset + Offset }.

calculateNewOffset(Deviations) ->
  DeviationSum = lists:sum(Deviations),
  round(DeviationSum / length(Deviations)).

unknown(State, Any) ->
  io:fwrite("Received unknown message ~p~n", [Any]),
  State.