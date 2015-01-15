-module(sender).
-export([start/6]).

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
  send_time=nil
}).


start(SyncManager, SlotManager, Interface, MulticastIP, Port, StationType) ->
  State = #s{
    sync_manager=SyncManager, 
    slot_manager=SlotManager,
    interface=Interface,
    multicast_ip=MulticastIP,
    port=Port,
    station_type=StationType
  },
  spawn(fun() -> loop(State) end).

loop(State) ->
  receive 
    {data, IncomingData} -> 
      loop(State#s{data=IncomingData});
    {new_timer, WaitTime} -> 
      cancelTimer(State#s.timer),
      NewState = State#s{
        timer=createTimer(WaitTime, {send}),
        send_time=?U:currentTime(State#s.sync_manager) + WaitTime
      },
      loop(NewState);
    
    {reserved_slot, Slot} ->
    io:format("~p sender:reserved_slot: ~p~n", [self(), Slot]),
      CurrentTime = ?U:currentTime(State#s.sync_manager),

      send(CurrentTime, Slot, State),
      loop(State);
    {send} ->
      io:format("sender:send~n", []),
      State#s.slot_manager ! {self(), reserve_slot},
      loop(State)
  end.

send(CurrentTime, Slot, State)
when CurrentTime < abs(State#s.send_time) + ?DELAY_TOLERANCE_IN_MS ->
  Socket = werkzeug:openSe(State#s.interface, State#s.port),
  Packet = buildPackage(State, Slot),
  io:format("sending packet~n", []),
  ok = gen_udp:send(Socket, State#s.multicast_ip, State#s.port, Packet);
send(_CurrentTime, _Slot, State) ->
  State#s.slot_manager ! {slot_missed}.

buildPackage(State, _Slot) when State#s.data == nil -> 
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