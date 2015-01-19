-module(sender).
-export([start/6]).

-define(DELAY_TOLERANCE_MS, 20).
-define(U, util).

-record(s, {
  sync_manager,
  slot_manager,
  multicast_ip,
  port,
  station_type,
  data,
  timer,
  send_time,
  socket
}).

start(SyncManager, SlotManager, Interface, MultiIP, Port, StationType) ->
  spawn(fun() -> init(
    SyncManager, SlotManager, Interface, MultiIP, Port, StationType
  ) end).

init(SyncManager, SlotManager, Interface, MultiIP, Port, StationType) ->
  Socket = werkzeug:openSe(Interface, Port),
  State = #s{
    socket = Socket,
    multicast_ip = MultiIP,
    port = Port,
    sync_manager = SyncManager,
    slot_manager = SlotManager,
    station_type = StationType
  },
  loop(State).

loop(State) ->
  receive
    {data, IncomingData}  -> loop(data(State, IncomingData));
    {new_timer, WaitTime} -> loop(newTimer(State, WaitTime));
    {reserved_slot, Slot} -> loop(reservedSlot(State, Slot));
    {send}                -> loop(send(State))
  end.

data(State, IncomingData) ->
  State#s{ data = IncomingData }.

newTimer(State, WaitTime) ->
  cancelTimer(State#s.timer),
  State#s{
    timer = createTimer(WaitTime, {send}),
    send_time = ?U:currentTime(State#s.sync_manager) + WaitTime
  }.

send(State) ->
  State#s.slot_manager ! {self(), reserve_slot},
  State.

reservedSlot(State, Slot) ->
  SendTime = State#s.send_time,
  CurrentTime = ?U:currentTime(State#s.sync_manager),
  doSend(State, SendTime, CurrentTime, Slot),
  State.

createTimer(WaitTime, Msg) when WaitTime < 0 ->
  createTimer(0, Msg);
createTimer(WaitTime, Msg) ->
  erlang:send_after(WaitTime, self(), Msg).

cancelTimer(undefined) -> ok;
cancelTimer(Timer) -> erlang:cancel_timer(Timer).

doSend(State, SendTime, CurrentTime, Slot)
when CurrentTime < abs(SendTime) + ?DELAY_TOLERANCE_MS ->
  Packet = buildPacket(State, Slot),
  ok = gen_udp:send(State#s.socket, State#s.multicast_ip, State#s.port, Packet);
doSend(State, _, _, _) ->
  State#s.slot_manager ! {slot_missed}.

buildPacket(State, _) when State#s.data == undefined ->
  State#s.slot_manager ! {slot_missed}; 
buildPacket(State, Slot) ->
  Data = list_to_binary (State#s.data),
  StationType = list_to_binary (State#s.station_type),
  SendTime = ?U:currentTime(State#s.sync_manager),

  <<StationType:1/binary,
    Data:24/binary,
    Slot:8/integer,
    SendTime:64/integer-big>>.
