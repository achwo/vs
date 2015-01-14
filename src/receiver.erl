-module(receiver).
-export([start/6]).


start(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port) ->
  spawn(fun() -> init(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port) end).

init(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port) ->
  udp_proc:start(self(), Interface, MultiIP, Port),
  loop(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port).


loop(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port) ->
  receive 

    {slot_end} -> 
      slotEnd(),
      loop(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port)
  end.

slotEnd() ->
  
  % if 0 messages -> answer {no_message}
  % if 1 message -> handle incoming msg
  %   answer {reserve_slot, SlotNumber}
  % else -> answer {collision}
  todo.