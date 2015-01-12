-module(slot_manager).
-export([start/0]).

start() -> spawn(fun() -> init() end).

init() ->
  % set sender and receiver
  % reset FreeSlotList
  % start SlotTimer
  loop().


loop() ->
  receive 
    {reserve_slot, SlotNumber} -> 
      reserveSlot(SlotNumber),
      loop();
    {From, reserve_slot} ->
      reserveRandomSlot(From),
      loop();
    {slot_end} ->
      slotEnd(),
      loop()
  end.


reserveSlot(SlotNumber) ->
  % remove SlotNumber from FreeSlotList (if possible)
  todo.

reserveRandomSlot(From) -> 
  % get random Slot from FreeSlotList and return
  % From ! {reserved_slot, SlotNumber}
  todo.

slotEnd() -> 
  % receiver ! {slot_end}
  % possible answers:
  % - collision
  % - no_message
  % - {reserve_slot, 9}

  % start new slottimer
  % if first slot, handleFrameEnd()

  todo.

handleFrameEnd() ->
  % sync_manager ! {sync},
  % sync_manager ! {reset_deviations}

  % transmission_slot setzen
  % sender ! {new_timer, WaitTimeTillTransmissionSlot}
  % reset FreeSlotList
  todo.




%{reserve_slot, SlotNumber} -> void: SlotChooser wird über bereits reservierten Slot im nächsten Frame informiert.
%{reserve_slot} -> {reserved_slot, SlotNumber}: Reserviert einen beliebigen Slot.
%{slot_end} -> void: Der SlotManager sendet sich via Timer eine Nachricht, sobald ein Slotende erreicht ist.
