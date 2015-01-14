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
  NewState = startSlotTimer(State, currentTime(State#s.sync_manager)),
  loop(NewState#s{free_slots=free_slot_list:new(?NUMBER_SLOTS)});
init(State) ->
  receive
    {set_sender, SenderPID} -> 
      NewState = State#s{sender=SenderPID};
    {set_receiver, ReceiverPID} ->
      NewState = State#s{receiver=ReceiverPID}
  end,
  init(NewState). 

loop(State) ->
  receive 
    {reserve_slot, Slot} -> loop(reserveSlot(Slot, State));
    {From, reserve_slot} -> loop(reserveRandomSlot(From, State));
    {slot_end}           -> loop(slotEnd(State))
  end.

reserveSlot(Slot, State) ->
  {ReservedSlot, List} = free_slot_list:reserveSlot(Slot, State#s.free_slots),
  State#s{
    reserved_slot = ReservedSlot,
    free_slots = List
  }.

reserveRandomSlot(From, State) -> 
  {Slot, List} = free_slot_list:reserveRandomSlot(State#s.free_slots),
  From ! {reserved_slot, Slot},
  State#s{free_slots=List}.

slotEnd(State) -> 
  NewState = checkSlotInbox(State),
  CurrentTime = currentTime(State#s.sync_manager),

  case currentSlot(CurrentTime) of
    1 -> NewNewState = handleFrameEnd(NewState); % todo: 0 or 1? | use result
    _ -> NewNewState = NewState
  end,
  startSlotTimer(NewNewState, CurrentTime).

checkSlotInbox(State) ->
  State#s.receiver ! {slot_end},
  receive
    {collision} ->
      State;  % todo really nothing else? 
    {no_message} ->
      State;  % todo really nothing else?
    {reserve_slot, SlotNumber} ->
      State#s{
        free_slots=free_slot_list:reserveSlot(SlotNumber, State#s.free_slots)
      }
  end.

% returns erlang timer
startSlotTimer(State, CurrentTime) ->
  case State#s.timer == nil of
    false -> erlang:cancel_timer(State#s.timer);
    _ -> ok
  end,
  WaitTime = timeTillNextSlot(CurrentTime),
  State#s{timer=erlang:send_after(WaitTime, self(), {slot_end})}.

% Changes ReservedSlot, FreeSlotList
handleFrameEnd(State) ->
  SyncManager = State#s.sync_manager,

  FrameBeforeSync = currentFrame(currentTime(SyncManager)),
  SyncManager ! {sync},
  SyncManager ! {reset_deviations},
  FrameAfterSync = currentFrame(currentTime(SyncManager)),

  % todo: maybe this block is fucked, because my brain is right now:
  case FrameBeforeSync > FrameAfterSync of 
    true -> 
      % because of sync we are still in the old frame
      State;
    false -> 
      {TransmissionSlot, NewState} = transmissionSlot(State), 
      NewState#s.sender ! {new_timer, timeTillTransmission(TransmissionSlot, currentTime(SyncManager))},
      resetSlots(State)
  end.

resetSlots(State) ->
  State#s{
    free_slots = free_slot_list:new(?NUMBER_SLOTS),
    reserved_slot = nil % todo: richtig?
  }.

transmissionSlot(State) when State#s.reserved_slot == nil ->
  {Slot, List} = free_slot_list:reserveLastFreeSlot(State#s.free_slots),
  {Slot, State#s{free_slots=List}};
transmissionSlot(State) ->
  {nil, State}.