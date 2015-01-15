-module(receiver).
-export([start/6]).

-record(m, {
  data,
  station_type,
  slot,
  send_time,
  receive_time
}).

-record(s, {
  sink,
  slot_manager,
  sync_manager,
  interface,
  multicast_ip,
  port,
  msg=nil,
  msg_count=0
}).

start(Sink, SlotManager, SyncManager, Interface, MultiIP, Port) ->
  State = #s{
    sink=Sink,
    slot_manager=SlotManager,
    sync_manager=SyncManager,
    interface=Interface,
    multicast_ip=MultiIP,
    port=Port
  },
  spawn(fun() -> init(State) end).

init(State) ->
  Receiver = self(),
  spawn(fun() -> 
    socketInit(Receiver, State#s.interface, State#s.multicast_ip, State#s.port) 
  end),
  loop(State).

loop(State) ->
  receive 
    {message, Data, StationType, Slot, SendTime} -> 
      io:format("receiver:message~n", []),
      NewState = State#s{
        msg = #m{
          data = Data,
          station_type = StationType,
          slot = Slot,
          send_time = SendTime,
          receive_time = util:currentTime(State#s.sync_manager)
        },
        msg_count = State#s.msg_count + 1
      },
      loop(NewState);
    {slot_end} -> 
      io:format("receiver:slot_end~n", []),
      loop(slotEnd(State))
  end.

slotEnd(State) ->
  case State#s.msg_count of
    0 -> 
      State#s.slot_manager ! {no_message};
    1 ->
      State#s.slot_manager ! {reserve_slot, State#s.msg#m.slot},
      State#s.sync_manager ! 
        {add_deviation, 
          State#s.msg#m.station_type, 
          State#s.msg#m.send_time, 
          State#s.msg#m.receive_time
        },
      State#s.sink ! {data, State#s.msg#m.data};
    _ -> 
      State#s.slot_manager ! {collision}
  end,
  reset(State).

reset(State) ->
  State#s{
    msg = nil,
    msg_count = 0
  }.

socketInit(Parent, Interface, MultiIP, Port) ->
  Socket = werkzeug:openRec(MultiIP, Interface, Port),
  gen_udp:controlling_process(Socket, self()),
  socketLoop(Parent, Socket).

socketLoop(Parent, Socket) ->
io:format("socketLoop~n", []),
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
