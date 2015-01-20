-module(receiver).
-export([start/6]).

-define(U, util).

-record(s, {
  sync_manager,
  slot_manager,
  sink,
  msg,
  msg_count = 0
}).

-record(m, {
  data,
  station_type,
  slot,
  send_time,
  receive_time
}).

start(Sink, SlotManager, SyncManager, Interface, MultiIP, Port) ->
  spawn(fun() -> 
    init(Sink, SlotManager, SyncManager, Interface, MultiIP, Port) end).

init(Sink, SlotManager, SyncManager, Interface, MultiIP, Port) ->
  State = #s{
    sync_manager = SyncManager,
    slot_manager = SlotManager,
    sink = Sink
  },
  rcv:start(self(), Interface, MultiIP, Port),
  loop(State).

loop(State) ->
  receive 
    {message, Data, StationType, Slot, SendTime} ->
      loop(message(Data, StationType, Slot, SendTime, State));
    {slot_end} ->
      loop(slotEnd(State));
    Any ->
      loop(unknown(Any, State))
  end.

message(Data, StationType, Slot, SendTime, State) ->
  State#s{
    msg = #m{
      data = Data,
      station_type = StationType,
      slot = Slot,
      send_time = SendTime,
      receive_time = ?U:currentTime(State#s.sync_manager)
    },
    msg_count = State#s.msg_count + 1
  }.

unknown(Any, State) ->
  io:fwrite("Received unknown message ~p~n", [Any]),
  State.

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
    msg = undefined,
    msg_count = 0
  }.