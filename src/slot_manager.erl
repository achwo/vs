-module(slot_manager).
-export([start/2]).
-import(log, [log/4, debug/4, nl/1]).

-define(NUMBER_SLOTS, 25).
-define(TRANSMISSION_TIME_OFFSET, 10).
-define(U, util).
-define(L, free_slot_list).

-record(s, {
  timer=nil, 
  sender=nil, 
  receiver=nil, 
  sync_manager=nil,
  reserved_slot=nil,
  free_slots=nil,
  transmission_slot=nil,
  log
}).

start(SyncManager, Log) -> 
  State = #s{
    sync_manager=SyncManager,
    log=Log
  },
  spawn(fun() -> init(State) end).

init(State) when State#s.sender /= nil, State#s.receiver /= nil ->
  log(State#s.log, "Initializing...", []),
  random:seed(now()),
  nl(State#s.log),
  debug(State#s.log, "init done", []),
  loop(resetSlots(startSlotTimer(State, ?U:currentTime(State#s.sync_manager))));
init(State) ->
  receive
    {set_sender, SenderPID}     -> init(State#s{sender=SenderPID});
    {set_receiver, ReceiverPID} -> init(State#s{receiver=ReceiverPID})
  end.

loop(State) ->
  receive 
    {Sender, reserve_slot} -> loop(reserveRandomSlot(Sender, State));
    {slot_end}           -> loop(slotEnd(State));
    {slot_missed}        -> loop(slotMissed(State))
  end.

reserveRandomSlot(Sender, State) -> 
  debug(State#s.log, "~p: reserveRandomSlot", [Sender]),
  {Slot, List} = ?L:reserveRandomSlot(State#s.free_slots),
  Sender ! {reserved_slot, Slot}, 
  State#s{free_slots=List, reserved_slot=Slot}.

slotEnd(State) -> 
  nl(State#s.log),
  debug(State#s.log, "slotEnd: ~p", [?U:currentSlot(?U:currentTime(State#s.sync_manager)) - 1]),
  debug(State#s.log, "time: ~p", [?U:currentTime(State#s.sync_manager)]),
  NewState = checkSlotInbox(State),
  CurrentTime = ?U:currentTime(State#s.sync_manager),

  case ?U:currentSlot(CurrentTime) of
    1 -> NewNewState = handleFrameEnd(NewState);
    _ -> NewNewState = NewState
  end,
  startSlotTimer(NewNewState, CurrentTime).

slotMissed(State) ->
  unsetReservation(State).

unsetReservation(State) when State#s.reserved_slot /= nil ->
  State#s{
    free_slots = ?L:readdReservedSlot(State#s.reserved_slot, State#s.free_slots),
    reserved_slot = nil
  };
unsetReservation(State) ->
  State.

checkSlotInbox(State) ->
  debug(State#s.log, "checkSlotInbox", []),
  State#s.receiver ! {slot_end},
  receive
    {collision} ->
      debug(State#s.log, "collision", []),
      % handleCollision(State);
      unsetReservation(State);
    {no_message} ->
      debug(State#s.log, "no message", []),
      State;
    {reserve_slot, Slot} -> % used by receiver
      debug(State#s.log, "reserveSlot: ~p", [Slot]),
      reserveSlot(Slot, State)
  end.

reserveSlot(Slot, State) ->
  debug(State#s.log, "reserveSlot: ~p", [Slot]),

  case State#s.reserved_slot of
    Slot -> % reservation of the slot we have reserved
      CurrentTime = ?U:currentTime(State#s.sync_manager),
      PreviousSlot = ?U:currentSlot(CurrentTime) - 1,
      isOwnReservationMessage(State, PreviousSlot, State#s.transmission_slot);
    _ ->
      State#s{ free_slots = ?L:reserveSlot(Slot, State#s.free_slots) }
  end.

isOwnReservationMessage(State, 0, TransmissionSlot) ->
  isOwnReservationMessage(State, 25, TransmissionSlot);
isOwnReservationMessage(State, TransmissionSlot, TransmissionSlot) ->
  % is our own reservation
  State;
isOwnReservationMessage(State, _PreviousSlot, _TransmissionSlot) ->
  State#s{ reserved_slot = nil }.

handleCollision(State) ->
  CurrentTime = ?U:currentTime(State#s.sync_manager),
  PreviousSlot = ?U:currentSlot(CurrentTime) - 1,
  collisionWithOwnMessage(State, State#s.transmission_slot, PreviousSlot).

collisionWithOwnMessage(State, Slot, 0) ->
  collisionWithOwnMessage(State, Slot, 25);
collisionWithOwnMessage(State, Slot, Slot) 
  when State#s.reserved_slot /= nil ->
  unsetReservation(State);
collisionWithOwnMessage(State, _, _) -> State.

% returns erlang timer
startSlotTimer(State, CurrentTime) ->
  debug(State#s.log, "startSlotTimer", []),
  case State#s.timer of
    nil -> ok;
    _ -> erlang:cancel_timer(State#s.timer)
  end,
  WaitTime = ?U:timeTillNextSlot(CurrentTime),
  debug(State#s.log, "currentTime: ~p", [CurrentTime]),
  State#s{timer=erlang:send_after(WaitTime, self(), {slot_end})}.

% Changes ReservedSlot, FreeSlotList
handleFrameEnd(State) ->
  debug(State#s.log, "handleFrameEnd", []),
  SyncManager = State#s.sync_manager,

  FrameBeforeSync = ?U:currentFrame(?U:currentTime(SyncManager)),
  SyncManager ! {sync},
  SyncManager ! {reset_deviations},
  FrameAfterSync = ?U:currentFrame(?U:currentTime(SyncManager)),

  debug(State#s.log, "FrameSyncDiff: ~p", [FrameAfterSync - FrameBeforeSync]),

  % todo: maybe this block is fucked, because my brain is right now:
  case FrameBeforeSync > FrameAfterSync of 
    true -> 
    debug(State#s.log, "sync time: old frame", []),
      % because of sync we are still in the old frame
      State;
    false -> 
    debug(State#s.log, "sync time: ok", []),
      TransmissionSlot = transmissionSlot(State), 
      debug(State#s.log, "transmissionSlot: ~p", [TransmissionSlot]),
      TimeTillTransmission = ?TRANSMISSION_TIME_OFFSET 
        + ?U:timeTillTransmission(TransmissionSlot, ?U:currentTime(SyncManager)),
      debug(State#s.log, "TimeTillTransmission: ~p", [TimeTillTransmission]),
      State#s.sender ! {new_timer, TimeTillTransmission},
      NewState = resetSlots(State),
      NewState#s {transmission_slot = TransmissionSlot}
  end.

resetSlots(State) ->
  debug(State#s.log, "resetSlots", []),
  State#s{
    free_slots = ?L:new(?NUMBER_SLOTS),
    reserved_slot = nil
  }.

transmissionSlot(State) when State#s.reserved_slot == nil ->
  % find a slot for use in current frame
  CurrentSlot = ?U:currentSlot(?U:currentTime(State#s.sync_manager)),
  FutureSlots = ?L:slotsAfter(CurrentSlot, State#s.free_slots),
  {Slot, _List} = ?L:reserveRandomSlot(FutureSlots),
  Slot;
transmissionSlot(State) ->
  debug(State#s.log, "having reserved_slot ", []),
  State#s.reserved_slot.

log(Log, Msg, Args) ->
  {_, {Module, _Function, _Arity}} = process_info(self(), current_function),
  log(Log, Module, Msg, Args).

debug(Log, Msg, Args) ->
  {_, {Module, _Function, _Arity}} = process_info(self(), current_function),
  debug(Log, Module, Msg, Args).