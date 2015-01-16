-module(sender).
-export([start/7]).
-import(log, [log/4, debug/4]).

-define(U, util).
-define(DELAY_TOLERANCE_IN_MS, 20).

-record(s, {
  sync_manager=nil,
  slot_manager=nil,
  interface=nil,
  multicast_ip=nil,
  port=nil,
  station_type=nil,
  data=nil,
  timer=nil,
  send_time=nil,
  socket=nil,
  log=nil
}).


start(SyncManager, SlotManager, Interface, MulticastIP, Port, StationType, Log) ->
  Socket = werkzeug:openSe(Interface, Port),
  State = #s{
    sync_manager=SyncManager, 
    slot_manager=SlotManager,
    interface=Interface,
    multicast_ip=MulticastIP,
    port=Port,
    station_type=StationType,
    socket = Socket,
    log=Log
  },
  spawn(fun() -> loop(State) end).

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
    timer=createTimer(WaitTime, {send}),
    send_time=?U:currentTime(State#s.sync_manager) + WaitTime
  }.

reservedSlot(State, Slot) ->
  doSend(State, ?U:currentTime(State#s.sync_manager), Slot),
  State.

send(State) ->
  State#s.slot_manager ! {self(), reserve_slot},
  State.

doSend(State, CurrentTime, Slot)
when CurrentTime < abs(State#s.send_time) + ?DELAY_TOLERANCE_IN_MS ->
  % Socket = werkzeug:openSe(State#s.interface, State#s.port),
  Packet = buildPackage(State, Slot),
  % ok = gen_udp:send(Socket, State#s.multicast_ip, State#s.port, Packet);
  ok = gen_udp:send(State#s.socket, State#s.multicast_ip, State#s.port, Packet);
doSend(State, _CurrentTime, _Slot) ->
  State#s.slot_manager ! {slot_missed}.

buildPackage(State, _Slot) when State#s.data == nil -> 
  debug(State#s.log, "No Data for Package!", []),
  State#s.slot_manager ! {slot_missed};
buildPackage(State, Slot) ->
  DataForPackage = list_to_binary(State#s.data),
  StationTypeForPackage = list_to_binary(State#s.station_type),
  Timestamp = ?U:currentTime(State#s.sync_manager),

  <<StationTypeForPackage:1/binary,
    DataForPackage:24/binary,
    Slot:8/integer,
    Timestamp:64/integer-big>>.


createTimer(WaitTime, Msg) when WaitTime < 0 ->
  createTimer(0, Msg);
createTimer(WaitTime, Msg) ->
  erlang:send_after(WaitTime, self(), Msg).

cancelTimer(nil) ->
  ok;
cancelTimer(Timer) ->
  erlang:cancel_timer(Timer).

% log(Log, Msg, Args) ->
%   {_, {Module, _Function, _Arity}} = process_info(self(), current_function),
%   log(Log, Module, Msg, Args).

debug(Log, Msg, Args) ->
  {_, {Module, _Function, _Arity}} = process_info(self(), current_function),
  debug(Log, Module, Msg, Args).