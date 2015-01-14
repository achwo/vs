-module(slot_manager).
-export([start/1]).

-define(FRAME_LENGTH_MS, 1000).
-define(NUMBER_SLOTS, 25).
-define(SLOT_LENGTH_MS, 40).

start(SyncManager) -> spawn(fun() -> init(SyncManager, nil, nil) end).

init(SyncManager, Sender, Receiver) when Sender /= nil, Receiver /= nil ->
  Timer = startSlotTimer(nil, currentTime(SyncManager)),
  loop(SyncManager, Sender, Receiver, resetFreeSlotList(), Timer);
init(SyncManager, Sender, Receiver) ->
  receive
    {set_sender, SenderPID} -> 
      init(SyncManager, SenderPID, Receiver);
    {set_receiver, ReceiverPID} ->
      init(SyncManager, Sender, ReceiverPID)
  end. 

currentTime(SyncManager) ->
  SyncManager ! {self(), get_current_time},
  receive 
    {current_time, Time} -> Time
  end.

% returns erlang timer
startSlotTimer(Timer, CurrentTime) ->
  case Timer == nil of
    false -> erlang:cancel_timer(Timer);
    _ -> ok
  end,

  WaitTime = timeTillNextSlot(CurrentTime),
  erlang:send_after(WaitTime, self(), {slot_end}).

timeTillNextSlot(CurrentTime) ->
  ?SLOT_LENGTH_MS - (CurrentTime rem ?SLOT_LENGTH_MS).

loop(SyncManager, Sender, Receiver, FreeSlotList, Timer) ->
  receive 
    {reserve_slot, SlotNumber} -> 
      NewFreeSlotList = reserveSlot(SlotNumber, FreeSlotList),
      loop(SyncManager, Sender, Receiver, NewFreeSlotList, Timer);
    {From, reserve_slot} ->
      NewFreeSlotList = reserveRandomSlot(From, FreeSlotList),
      loop(SyncManager, Sender, Receiver, NewFreeSlotList, Timer);
    {slot_end} ->
      NewFreeSlotList = slotEnd(Timer, Receiver, FreeSlotList, SyncManager),
      loop(SyncManager, Sender, Receiver, NewFreeSlotList, Timer)
  end.


reserveSlot(SlotNumber, FreeSlotList) ->
  lists:delete(SlotNumber, FreeSlotList).

reserveRandomSlot(From, FreeSlotList) -> 
  Index = random:uniform(length(FreeSlotList)),
  Slot = lists:nth(Index, FreeSlotList),
  NewFreeSlotList = lists:delete(Slot, FreeSlotList),
  From ! {reserved_slot, Slot},
  NewFreeSlotList.

slotEnd(Timer, Receiver, FreeSlotList, SyncManager) -> 
  Receiver ! {slot_end},
  receive
    {collision} ->
      NewFreeSlotList = FreeSlotList;  % todo really nothing else? 
    {no_message} ->
      NewFreeSlotList = FreeSlotList;  % todo really nothing else?
    {reserve_slot, SlotNumber} ->
      NewFreeSlotList = reserveSlot(SlotNumber, FreeSlotList)
  end,

  CurrentTime = currentTime(SyncManager),
  startSlotTimer(Timer, CurrentTime),
  case currentSlot(CurrentTime) of
    1 -> handleFrameEnd();
    _ -> nothing
  end,
  NewFreeSlotList.

currentSlot(CurrentTime) ->
  currentFrameTime(CurrentTime) rem ?SLOT_LENGTH_MS. % todo: slot# base 0 or 1?

currentFrameTime(CurrentTime) ->
  CurrentTime rem ?FRAME_LENGTH_MS.

handleFrameEnd() ->
  % sync_manager ! {sync},
  % sync_manager ! {reset_deviations}

  % transmission_slot setzen
  % sender ! {new_timer, WaitTimeTillTransmissionSlot}
  resetFreeSlotList(),
  todo.

resetFreeSlotList() ->
  createFreeSlotList([], 0, ?NUMBER_SLOTS).

createFreeSlotList(List, CurrentSlotNumber, TotalNumber) 
  when CurrentSlotNumber >= TotalNumber ->
  List;
createFreeSlotList(List, CurrentSlotNumber, TotalNumber) ->
  createFreeSlotList([CurrentSlotNumber | List], CurrentSlotNumber + 1, TotalNumber).