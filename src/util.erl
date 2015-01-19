-module(util).
-export([currentTime/1, currentSlot/1, currentFrameTime/1, timeTillNextSlot/1,
  currentFrame/1, timeTillTransmission/2, time_till_next_frame/1,
  previousSlot/1]).

-export([transmissionTime/2]).

-define(FRAME_LENGTH_MS, 1000).
-define(NUMBER_SLOTS, 25).
-define(SLOT_LENGTH_MS, 40).

currentTime(SyncManager) ->
  SyncManager ! {self(), get_current_time},
  receive 
    {current_time, Time} -> Time
  end.

currentSlot(CurrentTime) ->
  (currentFrameTime(CurrentTime) div ?SLOT_LENGTH_MS) + 1.

previousSlot(CurrentTime) ->
  case (currentSlot(CurrentTime) - 1) of
    0 -> 25;
    N -> N
  end.  

currentFrameTime(CurrentTime) ->
  CurrentTime rem ?FRAME_LENGTH_MS.

timeTillNextSlot(CurrentTime) ->
  ?SLOT_LENGTH_MS - (CurrentTime rem ?SLOT_LENGTH_MS).

currentFrame(Time) ->
  Time div ?FRAME_LENGTH_MS.

time_till_next_frame (CurrentTime) ->
  ?FRAME_LENGTH_MS - currentFrameTime (CurrentTime).

transmissionTime(TransmissionSlot, Frame) ->
  (?SLOT_LENGTH_MS * (TransmissionSlot - 1)) + Frame * ?FRAME_LENGTH_MS.

timeTillTransmission(TransmissionSlot, CurrentTime) ->
    transmissionTime(TransmissionSlot, currentFrame(CurrentTime)) - CurrentTime.