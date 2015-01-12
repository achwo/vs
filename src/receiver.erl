-module(receiver).
-export([start/0]).

start() -> spawn(fun() -> loop() end).

loop() ->
  receive 
    {slot_end} -> 
      slotEnd(),
      loop()
  end.

slotEnd() ->
  % if 0 messages -> answer {no_message}
  % if 1 message -> handle incoming msg
  %   answer {reserve_slot, SlotNumber}
  % else -> answer {collision}
  todo.