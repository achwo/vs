-module(receiver).
-export([start/0]).


start(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port) ->
  spawn(fun() -> init(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port) end).

init(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port) ->
  udp_proc:start(self(), Interface, MultiIP, Port),
  loop(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port).


loop(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port) ->
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