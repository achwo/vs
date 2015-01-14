-module(slot_manager).
-export([start/1]).

-import(util, [currentTime/1, currentSlot/1, timeTillNextSlot/1, currentFrame/1,
  timeTillTransmission/2]).

-define(NUMBER_SLOTS, 25).

start(SyncManager) -> spawn(fun() -> init(SyncManager, nil, nil) end).

init(SyncManager, Sender, Receiver) when Sender /= nil, Receiver /= nil ->
  Timer = startSlotTimer(nil, currentTime(SyncManager)),
  loop(nil, SyncManager, Sender, Receiver, free_slot_list:new(?NUMBER_SLOTS), Timer); % todo: ReservedSlot = nil ok?
init(SyncManager, Sender, Receiver) ->
  receive
    {set_sender, SenderPID} -> 
      init(SyncManager, SenderPID, Receiver);
    {set_receiver, ReceiverPID} ->
      init(SyncManager, Sender, ReceiverPID)
  end. 

loop(ReservedSlot, SyncManager, Sender, Receiver, FreeSlotList, Timer) ->
  receive 
    {reserve_slot, SlotNumber} -> 
      {NewReservedSlot, NewFreeSlotList} = free_slot_list:reserveSlot(SlotNumber, FreeSlotList),
      loop(NewReservedSlot, SyncManager, Sender, Receiver, NewFreeSlotList, Timer);
    {From, reserve_slot} ->
      NewFreeSlotList = reserveRandomSlot(From, FreeSlotList),
      loop(ReservedSlot, SyncManager, Sender, Receiver, NewFreeSlotList, Timer);
    {slot_end} ->
      NewFreeSlotList = slotEnd(Timer, Receiver, FreeSlotList, SyncManager, ReservedSlot, Sender),
      loop(ReservedSlot, SyncManager, Sender, Receiver, NewFreeSlotList, Timer)
  end.

% changes FreeSlotList
reserveRandomSlot(From, FreeSlotList) -> 
  {Slot, NewFreeSlotList} = free_slot_list:reserveRandomSlot(FreeSlotList),
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
      NewFreeSlotList = free_slot_list:reserveSlot(SlotNumber, FreeSlotList)
  end,

  CurrentTime = currentTime(SyncManager),
  case currentSlot(CurrentTime) of
    1 -> handleFrameEnd(SyncManager, ReservedSlot, Sender, FreeSlotList); % todo: 0 or 1? | use result
    _ -> nothing
  end,
  startSlotTimer(Timer, CurrentTime),
  NewFreeSlotList.

% returns erlang timer
startSlotTimer(Timer, CurrentTime) ->
  case Timer == nil of
    false -> erlang:cancel_timer(Timer);
    _ -> ok
  end,

  WaitTime = timeTillNextSlot(CurrentTime),
  erlang:send_after(WaitTime, self(), {slot_end}).

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
      {TransmissionSlot, _FreeSlotList} = transmissionSlot(ReservedSlot, FreeSlotList), 
      Sender ! {new_timer, timeTillTransmission(TransmissionSlot, CurrentTime)},
      NewFreeSlotList = free_slot_list:new(?NUMBER_SLOTS),
      % todo: maybe this block is fucked, because my brain is right now
  end,
  {NewFreeSlotList, NewReservedSlot}.

transmissionSlot(nil, FreeSlotList) ->
  free_slot_list:reserveLastFreeSlot(FreeSlotList);
transmissionSlot(ReservedSlot, FreeSlotList) ->
  {ReservedSlot, FreeSlotList}.