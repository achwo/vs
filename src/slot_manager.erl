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
  spawn(fun() -> init(#s{sync_manager=SyncManager}) end).

init(State) when State#s.sender /= nil, State#s.receiver /= nil ->
  random:seed(now()),
  NewState = startSlotTimer(State, currentTime(State#s.sync_manager)),
  io:format("init(bla~n", []),
  loop(NewState#s{free_slots=free_slot_list:new(?NUMBER_SLOTS)});
init(State) ->
  receive
    {set_sender, SenderPID} -> 
      init(State#s{sender=SenderPID});
    {set_receiver, ReceiverPID} ->
      init(State#s{receiver=ReceiverPID})
  end.

loop(State) ->
  io:format("loop~n", []),
  receive 
    {reserve_slot, Slot} -> loop(reserveSlot(Slot, State));
    {From, reserve_slot} -> loop(reserveRandomSlot(From, State));
    {slot_end}           -> loop(slotEnd(State))
  end.

reserveSlot(Slot, State) ->
  io:format("reserveSlot~n", []),
  {ReservedSlot, List} = free_slot_list:reserveSlot(Slot, State#s.free_slots),
  State#s{
    reserved_slot = ReservedSlot,
    free_slots = List
  }.

reserveRandomSlot(From, State) -> 
  io:format("reserveRandomSlot~n", []),
  {Slot, List} = free_slot_list:reserveRandomSlot(State#s.free_slots),
  From ! {reserved_slot, Slot},
  State#s{free_slots=List}.

slotEnd(State) -> 
  io:format("slotEnd~n", []),
  NewState = checkSlotInbox(State),
  CurrentTime = currentTime(State#s.sync_manager),

  case currentSlot(CurrentTime) of
    1 -> NewNewState = handleFrameEnd(NewState); % todo: 0 or 1? | use result
    _ -> NewNewState = NewState
  end,
  startSlotTimer(NewNewState, CurrentTime).

checkSlotInbox(State) ->
  io:format("checkSlotInbox~n", []),
  State#s.receiver ! {slot_end},
  receive
    {collision} ->
      io:format("collision~n", []),
      State;  % todo really nothing else? 
    {no_message} ->
      io:format("no message~n", []),
      State;  % todo really nothing else?
    {reserve_slot, SlotNumber} ->
      io:format("reserveSlot: ~p~n", [SlotNumber]),
      State#s{
        free_slots=free_slot_list:reserveSlot(SlotNumber, State#s.free_slots)
      }
  end.

% returns erlang timer
startSlotTimer(State, CurrentTime) ->
  io:format("startSlotTimer~n", []),
  case State#s.timer of
    nil -> ok;
    _ -> erlang:cancel_timer(State#s.timer)
  end,
  WaitTime = timeTillNextSlot(CurrentTime),
  State#s{timer=erlang:send_after(WaitTime, self(), {slot_end})}.

% Changes ReservedSlot, FreeSlotList
handleFrameEnd(State) ->
  io:format("handleFrameEnd~n", []),
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
      resetSlots(NewState)
  end.

resetSlots(State) ->
  io:format("resetSlots~n", []),
  State#s{
    free_slots = free_slot_list:new(?NUMBER_SLOTS),
    reserved_slot = nil % todo: richtig?
  }.

transmissionSlot(State) when State#s.reserved_slot == nil ->
  {Slot, List} = free_slot_list:reserveLastFreeSlot(State#s.free_slots),
  io:format("transmissionSlot1: ~p~n", [Slot]),
  io:format("transmissionSlot1: ~p~n", [List]),
  {Slot, State#s{free_slots=List}};
transmissionSlot(State) ->
  io:format("transmissionSlot2 ~n", []),
  {nil, State}.