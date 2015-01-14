-module(receiver).
-export([start/6]).


start(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port) ->
  spawn(fun() -> init(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port) end).

init(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port) ->
  spawn(fun() -> socketInit(self(), Interface, MultiIP, Port) end),
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

socketInit(Parent, Interface, MultiIP, Port) ->
  Socket = werkzeug:openRec(MultiIP, Interface, Port),
  gen_udp:controlling_process(Socket, self()),
  socketLoop(Parent, Socket).

socketLoop(Parent, Socket) ->
  {ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, 0),
  <<StationType:1/binary,
    Payload:24/binary,
    Slot:8/integer,
    Timestamp:64/integer-big>> = Packet,
  Parent ! {message,
    binary_to_list (Payload),
    binary_to_list (StationType),
    Slot,
    Timestamp
  },
  socketLoop(Parent, Socket).
