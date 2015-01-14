-module(slot_manager).
-export([start/1]).

-define(NUMBER_SLOTS, 25).

start(SyncManager) -> spawn(fun() -> init(SyncManager, nil, nil) end).

init(SyncManager, Sender, Receiver) when Sender /= nil, Receiver /= nil ->
  % start SlotTimer
  loop(SyncManager, Sender, Receiver, resetFreeSlotList());
init(SyncManager, Sender, Receiver) ->
  receive
    {set_sender, SenderPID} -> 
      NewSender = SenderPID,
      init(SyncManager, NewSender, Receiver);
    {set_receiver, ReceiverPID} ->
      NewReceiver = ReceiverPID,
      init(SyncManager, Sender, NewReceiver)
  end. 


loop(SyncManager, Sender, Receiver, FreeSlotList) ->
  receive 
    {reserve_slot, SlotNumber} -> 
      NewFreeSlotList = reserveSlot(SlotNumber, FreeSlotList),
      loop(SyncManager, Sender, Receiver, NewFreeSlotList);
    {From, reserve_slot} ->
      NewFreeSlotList = reserveRandomSlot(From, FreeSlotList),
      loop(SyncManager, Sender, Receiver, NewFreeSlotList);
    {slot_end} ->
      NewFreeSlotList = slotEnd(Receiver, FreeSlotList),
      loop(SyncManager, Sender, Receiver, NewFreeSlotList)
  end.


reserveSlot(SlotNumber, FreeSlotList) ->
  lists:delete(SlotNumber, FreeSlotList).

reserveRandomSlot(From, FreeSlotList) -> 
  Index = random:uniform(length(FreeSlotList)),
  Slot = lists:nth(Index, FreeSlotList),
  NewFreeSlotList = lists:delete(Slot, FreeSlotList),
  From ! {reserved_slot, Slot},
  NewFreeSlotList.

slotEnd(Receiver, FreeSlotList) -> 
  Receiver ! {slot_end},
  receive
    {collision} ->
      nothing; % really?
    {no_message} ->
      nothing; % really?
    {reserve_slot, SlotNumber} ->
      NewFreeSlotList = reserveSlot(SlotNumber, FreeSlotList)
  end,
  % start new slottimer
  % if first slot, handleFrameEnd()

  % NewFreeSlotList.
  FreeSlotList.

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