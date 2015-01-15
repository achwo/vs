-module(receiver).
-export([start/6]).


start(Sink, SlotManager, SyncManager, Interface, MultiIP, Port) ->
  spawn(fun() -> init(Sink, SlotManager, SyncManager, Interface, MultiIP, Port) end).

init(Sink, SlotManager, SyncManager, Interface, MultiIP, Port) ->
  spawn(fun() -> socketInit(self(), Interface, MultiIP, Port) end),
  loop(0, nil, Sink, SlotManager, SyncManager, Interface, MultiIP, Port).


loop(MessageCount, ReceivedMessage, Sink, SlotManager, SyncManager, Interface, MultiIP, Port) ->
  receive 
    {message, Data, StationType, Slot, SendTime} -> 
      io:format("receiver:message~n", []),
      ReceiveTime = util:currentTime(SyncManager),
      NewReceivedMessage = {message, Data, StationType, Slot, SendTime, ReceiveTime},
      NewMessageCount = MessageCount + 1,
      loop(NewMessageCount, NewReceivedMessage, Sink, SlotManager, SyncManager, Interface, MultiIP, Port);
    {slot_end} -> 
      io:format("receiver:slot_end~n", []),
      {NewMessageCount, NewReceivedMessage} = slotEnd(MessageCount, ReceivedMessage, SlotManager, SyncManager, Sink),
      loop(NewMessageCount, NewReceivedMessage, Sink, SlotManager, SyncManager, Interface, MultiIP, Port)
  end.

slotEnd(MessageCount, ReceivedMessage, SlotManager, SyncManager, Sink) ->
  case MessageCount of
    0 -> 
      SlotManager ! {no_message};
    1 ->
      {Data, StationType, Slot, SendTime, ReceiveTime} = ReceivedMessage,
      SlotManager ! {reserve_slot, Slot},
      SyncManager ! {add_deviation, StationType, SendTime, ReceiveTime},
      Sink ! {data, Data};
    _ -> 
      SlotManager ! {collision}
  end,
  {0, nil}.

socketInit(Parent, Interface, MultiIP, Port) ->
  Socket = werkzeug:openRec(MultiIP, Interface, Port),
  gen_udp:controlling_process(Socket, self()),
  socketLoop(Parent, Socket).

socketLoop(Parent, Socket) ->
  {ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, 0),
  <<StationType:1/binary,
    Data:24/binary,
    Slot:8/integer,
    Timestamp:64/integer-big>> = Packet,
  Parent ! {message,
    binary_to_list (Data),
    binary_to_list (StationType),
    Slot,
    Timestamp
  },
  socketLoop(Parent, Socket).
