-module(slot_manager).
-export([start/1]).

-import(util, [currentTime/1, currentSlot/1, timeTillNextSlot/1, currentFrame/1,
  timeTillTransmission/2]).

-define(NUMBER_SLOTS, 25).

-record(s, {
  timer=nil, 
  sender=nil, 
  receiver=nil, 
  sync_manager=nil,
  reserved_slot=nil,
  free_slots=nil
}).

start(SyncManager) -> 
  State = #s{sync_manager=SyncManager},
  spawn(fun() -> init(State) end).

init(State) when State#s.sender /= nil, State#s.receiver /= nil ->

  NewState = State#s{
    timer=startSlotTimer(nil, currentTime(State#s.sync_manager)),
    free_slots=free_slot_list:new(?NUMBER_SLOTS)
  },
  loop(NewState);
init(State) ->
  receive
    {set_sender, SenderPID} -> 
      NewState = State#s{sender=SenderPID},
      init(NewState);
    {set_receiver, ReceiverPID} ->
      NewState = State#s{receiver=ReceiverPID},
      init(NewState)
  end. 

loop(State) ->
  receive 
    {reserve_slot, SlotNumber} -> 
      {NewReservedSlot, NewFreeSlotList} = free_slot_list:reserveSlot(SlotNumber, State#s.free_slots),
      NewState = State#s{
        reserved_slot = NewReservedSlot,
        free_slots = NewFreeSlotList
      },
      loop(NewState);
    {From, reserve_slot} ->
      NewState = State#s{free_slots = reserveRandomSlot(From, State#s.free_slots)},
      loop(NewState);
    {slot_end} ->
      NewFreeSlotList = slotEnd(State#s.timer, State#s.receiver, State#s.free_slots, State#s.sync_manager, State#s.reserved_slot, State#s.sender),
      NewState = State#s{free_slots=NewFreeSlotList},
      loop(NewState)
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
      NewReservedSlot = nil % todo: richtig?
      % todo: maybe this block is fucked, because my brain is right now
  end,
  {NewFreeSlotList, NewReservedSlot}.

transmissionSlot(nil, FreeSlotList) ->
  free_slot_list:reserveLastFreeSlot(FreeSlotList);
transmissionSlot(ReservedSlot, FreeSlotList) ->
  {ReservedSlot, FreeSlotList}.