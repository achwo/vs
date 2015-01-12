-module(sync_manager).
-export([start/0]).

start() -> spawn(fun() -> loop() end).

loop() ->
  receive 
    {add_deviation, StationType, SendTime, ReceiveTime} ->
      addDeviation(StationType, SendTime, ReceiveTime),
      loop();
    {reset_deviations} ->
      resetDeviations(),
      loop();
    {From, get_current_time} ->
      getCurrentTime(From),
      loop()
  end.

addDeviation(StationType, SendTime, ReceiveTime) ->
  % add deviation to list
  todo.

resetDeviations() ->
  % clear deviation list
  todo.

getCurrentTime(From) ->
  % From ! {current_time, CurrentTime}.
  todo.