-module(slot_manager).
-export([start/1]).

-define(U, util).
-define(L, free_slot_list).

-define(NUMBER_SLOTS, 25).
-define(TRANSMISSION_DELAY, 10).

-record(s, {
  sync_manager,
  sender,
  receiver,
  free_slots,
  reserved_slot = nil,
  timer = undefined,
  send_slot
}).


start(SyncManager) ->
  spawn(fun () -> getComponents(#s { sync_manager = SyncManager }) end).

getComponents(State) ->
  receive
    {set, Sender, Receiver} ->
      NewState = State#s { 
        sender = Sender,
        receiver = Receiver 
      }
  end,
  init(NewState).

init(State) ->
  T = ?U:currentTime(State#s.sync_manager),
  random:seed(now()),
  loop(resetSlots(startTimer(State, T))).

loop(State) ->
  receive
    {_Sender, reserve_slot} -> loop(reserveSlot(State));
    {slot_missed}           -> loop(unsetReservation(State));
    {slot_end}              -> loop(slotEnd(State))
  end.

reserveSlot(State) ->
  {NextSlot, NewList} = ?L:reserveRandomSlot(State#s.free_slots),
  State#s.sender ! {reserved_slot, NextSlot},
  State#s { 
    free_slots = NewList,
    reserved_slot = NextSlot
  }.

slotEnd(State) -> 
  State#s.receiver ! {slot_end},
  receive
    {no_message}          -> NewState = State;
    {reserve_slot, Slot}  -> NewState = reserveSlot(State, Slot);
    {collision}           -> NewState = collision(State)
  end,
  T = ?U:currentTime(NewState#s.sync_manager),
  CurrentSlot = ?U:currentSlot(T),

  case CurrentSlot of
    1 -> frameEnd(NewState, T);
    _ -> startTimer(NewState, T)
  end.

reserveSlot(State, Slot) ->
  case State#s.reserved_slot of
    Slot -> 
      case wasPreviousSlotOurSendSlot(State) of
        false -> unsetReservation(State);
        true  -> State
      end;
    _ ->
      State#s{ free_slots = ?L:reserveSlot(Slot, State#s.free_slots) }
  end.

collision(State) ->
  case wasPreviousSlotOurSendSlot(State) of
    true  -> unsetReservation(State);
    false -> State
  end.

wasPreviousSlotOurSendSlot(State) ->
  TransmissionSlot = State#s.send_slot,

  case ?U:previousSlot(?U:currentTime(State#s.sync_manager)) of 
    TransmissionSlot  -> true;
    _                 -> false
  end.

unsetReservation(State) when State#s.reserved_slot /= nil ->
  State#s{
    free_slots = ?L:readdReservedSlot(State#s.reserved_slot, State#s.free_slots),
    reserved_slot = nil
  };
unsetReservation(State) ->
  State.

frameEnd(State, PreSyncT) ->
  FrameBeforeSync = ?U:currentFrame(PreSyncT),
  sync(State),
  TimeAfterSync = ?U:currentTime(State#s.sync_manager),
  FrameAfterSync = ?U:currentFrame(TimeAfterSync),
  startTimer(
    prepareFrame(State, FrameBeforeSync, FrameAfterSync, TimeAfterSync),
    TimeAfterSync
  ).

sync(State) ->
  State#s.sync_manager ! {sync},
  State#s.sync_manager ! {reset_deviations}.

prepareFrame(State, FrameBeforeSync, FrameAfterSync, _)
  when FrameBeforeSync > FrameAfterSync ->
  State;
prepareFrame(State, _PreSyncFrame, _PostSyncFrame, TimeAfterSync) ->
  TransmissionSlot = sendSlot(State),
  informSender(State, TransmissionSlot, TimeAfterSync),
  NewState = resetSlots(State),
  NewState#s { send_slot = TransmissionSlot }.

sendSlot(State) ->
  case State#s.reserved_slot of
    nil -> 
      CurrentSlot = ?U:currentSlot(?U:currentTime(State#s.sync_manager)),
      FreeSlots = ?L:slotsAfter(CurrentSlot, State#s.free_slots),
      {Slot, _List} = ?L:reserveRandomSlot(FreeSlots);
    _ -> 
      Slot = State#s.reserved_slot
  end,
  Slot.

informSender(_Context, nil, _T) ->
  ok;
informSender(State, Slot, T) ->
  WaitTime = ?TRANSMISSION_DELAY + ?U:timeTillTransmission(Slot, T),
  State#s.sender ! {new_timer, WaitTime}.

startTimer(State, CurrentTime) ->
  case State#s.timer of
    undefined -> ok;
    _ -> erlang:cancel_timer(State#s.timer)
  end,
  WaitTime = ?U:timeTillNextSlot(CurrentTime),
  State#s{timer=erlang:send_after(WaitTime, self(), {slot_end})}.

% resets the list of free slots
resetSlots(State) ->
  State#s {
    free_slots = ?L:new(?NUMBER_SLOTS),
    reserved_slot = nil
  }.