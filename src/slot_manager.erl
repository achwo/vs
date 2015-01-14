-module(slot_manager).
-export([start/1]).

-import(util, [currentTime/1, currentSlot/1, timeTillNextSlot/1, currentFrame/1]).

-define(FRAME_LENGTH_MS, 1000).
-define(NUMBER_SLOTS, 25).
-define(SLOT_LENGTH_MS, 40).

start(SyncManager) -> spawn(fun() -> init(SyncManager, nil, nil) end).

init(SyncManager, Sender, Receiver) when Sender /= nil, Receiver /= nil ->
  Timer = startSlotTimer(nil, currentTime(SyncManager)),
  loop(nil, SyncManager, Sender, Receiver, resetFreeSlotList(), Timer); % todo: ReservedSlot = nil ok?
init(SyncManager, Sender, Receiver) ->
  receive
    {set_sender, SenderPID} -> 
      init(SyncManager, SenderPID, Receiver);
    {set_receiver, ReceiverPID} ->
      init(SyncManager, Sender, ReceiverPID)
  end. 

% returns erlang timer
startSlotTimer(Timer, CurrentTime) ->
  case Timer == nil of
    false -> erlang:cancel_timer(Timer);
    _ -> ok
  end,

  WaitTime = timeTillNextSlot(CurrentTime),
  erlang:send_after(WaitTime, self(), {slot_end}).

loop(ReservedSlot, SyncManager, Sender, Receiver, FreeSlotList, Timer) ->
  receive 
    {reserve_slot, SlotNumber} -> 
      NewFreeSlotList = reserveSlot(SlotNumber, FreeSlotList),
      loop(ReservedSlot, SyncManager, Sender, Receiver, NewFreeSlotList, Timer);
    {From, reserve_slot} ->
      NewFreeSlotList = reserveRandomSlot(From, FreeSlotList),
      loop(ReservedSlot, SyncManager, Sender, Receiver, NewFreeSlotList, Timer);
    {slot_end} ->
      NewFreeSlotList = slotEnd(Timer, Receiver, FreeSlotList, SyncManager, ReservedSlot, Sender),
      loop(ReservedSlot, SyncManager, Sender, Receiver, NewFreeSlotList, Timer)
  end.


% changes FreeSlotList
reserveSlot(SlotNumber, FreeSlotList) ->
  lists:delete(SlotNumber, FreeSlotList).

% changes FreeSlotList
reserveRandomSlot(From, FreeSlotList) -> 
  Index = random:uniform(length(FreeSlotList)),
  Slot = lists:nth(Index, FreeSlotList),
  NewFreeSlotList = lists:delete(Slot, FreeSlotList),
  From ! {reserved_slot, Slot},
  NewFreeSlotList.

% changes FreeSlotList, ReservedSlot
slotEnd(Timer, Receiver, FreeSlotList, SyncManager, ReservedSlot, Sender) -> 
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
  case currentSlot(CurrentTime) of
    1 -> handleFrameEnd(SyncManager, ReservedSlot, Sender, FreeSlotList); % todo: use result
    _ -> nothing
  end,
  startSlotTimer(Timer, CurrentTime),
  NewFreeSlotList.


% Changes ReservedSlot, FreeSlotList
handleFrameEnd(SyncManager, ReservedSlot, Sender, FreeSlotList) ->
  FrameBeforeSync = currentFrame(currentTime(SyncManager)),
  SyncManager ! {sync},
  SyncManager ! {reset_deviations},
  FrameAfterSync = currentFrame(currentTime(SyncManager)),

  case FrameBeforeSync > FrameAfterSync of 
    true -> 
      NewFreeSlotList = FreeSlotList,
      NewReservedSlot = ReservedSlot;
    false -> 
      CurrentTime = currentTime(SyncManager),
      TransmissionSlot = transmissionSlot(ReservedSlot, currentSlot(CurrentTime)),
      Sender ! {new_timer, timeTillTransmission(TransmissionSlot, CurrentTime)},
      NewFreeSlotList = resetFreeSlotList(),
      NewReservedSlot = nil

  end,
  {NewFreeSlotList, NewReservedSlot}.

transmissionSlot(nil, CurrentSlot) ->
  % reserve Slot > CurrentSlot
  CurrentSlot,
  todo;
transmissionSlot(ReservedSlot, _CurrentSlot) ->
  ReservedSlot.

timeTillTransmission(TransmissionSlot, Time) ->
  TransmissionSlot,
  Time,
  % time - transmissionSlotTime
  todo.

resetFreeSlotList() ->
  createFreeSlotList([], 0, ?NUMBER_SLOTS).

createFreeSlotList(List, CurrentSlotNumber, TotalNumber) 
  when CurrentSlotNumber >= TotalNumber ->
  List;
createFreeSlotList(List, CurrentSlotNumber, TotalNumber) ->
  createFreeSlotList([CurrentSlotNumber | List], CurrentSlotNumber + 1, TotalNumber).