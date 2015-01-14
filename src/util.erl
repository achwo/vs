-module(util).
-export([currentTime/1, currentSlot/1, currentFrameTime/1, timeTillNextSlot/1,
  currentFrame/1, timeTillTransmission/2]).

-define(FRAME_LENGTH_MS, 1000).
-define(NUMBER_SLOTS, 25).
-define(SLOT_LENGTH_MS, 40).

currentTime(SyncManager) ->
  SyncManager ! {self(), get_current_time},
  receive 
    {current_time, Time} -> Time
  end.

currentSlot(CurrentTime) ->
  currentFrameTime(CurrentTime) rem ?SLOT_LENGTH_MS. % todo: slot# base 0 or 1?

currentFrameTime(CurrentTime) ->
  CurrentTime rem ?FRAME_LENGTH_MS.

timeTillNextSlot(CurrentTime) ->
  ?SLOT_LENGTH_MS - (CurrentTime rem ?SLOT_LENGTH_MS).

currentFrame(Time) ->
  Time,
  todo.

timeTillTransmission(TransmissionSlot, Time) ->
  TransmissionSlot,
  Time,
  % time - transmissionSlotTime
  todo.