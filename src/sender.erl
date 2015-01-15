-module(sender).
-export([start/6]).

-define(U, util).
-define(DELAY_TOLERANCE_IN_MS, 20).

start(SyncManager, SlotManager, Interface, MultiIP, Port, StationType) ->
  spawn(fun() -> loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, data, timer, sendTime) end).

loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, Data, Timer, SendTime) ->
  receive 
    {data, IncomingData} -> 
    io:format("sender:data~n", []),
      NewData = data(IncomingData),
      loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, NewData, Timer, SendTime);
    
    {new_timer, WaitTime} -> 
    io:format("sender:new_timer~n", []),
      cancelTimer(Timer),
      NewTimer = createTimer(WaitTime, {send}),
      NewSendTime = ?U:currentTime(SyncManager) + WaitTime,
      loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, Data, NewTimer, NewSendTime);
    
    {reserved_slot, Slot} ->
    io:format("sender:reserved_slot:~p~n", [Slot]),
      CurrentTime = ?U:currentTime(SyncManager),
      send(CurrentTime, SendTime, Interface, Port, Data, StationType, SyncManager, Slot, MultiIP, SlotManager);
      
    {send} ->
      io:format("sender:send~n", []),
      requestSlot(SlotManager),
      loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, Data, Timer, SendTime)
  end.

data(Data) ->
  Data.

requestSlot(SlotManager) ->
  SlotManager ! {self(), reserve_slot}.



send(CurrentTime, SendTime, Interface, Port, Data, StationType, SyncManager, Slot, MultiIP, SlotManager)
when CurrentTime < abs(SendTime) + ?DELAY_TOLERANCE_IN_MS ->
  Socket = werkzeug:openSe(Interface, Port),
  Packet = buildPackage(Data, StationType, SyncManager, Slot, SlotManager),
  ok = gen_udp:send(Socket, MultiIP, Port, Packet);
send(_, _, _, _, _, _, _, _, _, SlotManager) ->
  SlotManager ! {slot_end}.

buildPackage(Data,_,_,SlotManager,_) when Data == undefined -> 
  SlotManager ! {slot_end};
buildPackage(Data, StationType, SyncManager, _, Slot) ->

  DataForPackage = list_to_binary(Data),
  StationTypeForPackage = list_to_binary(StationType),
  Timestamp = ?U:currentTime(SyncManager),
  io:format("sender:buildPackage() StationType: ~p~n", [StationType]),
  io:format("sender:buildPackage() StationTypeForPackage: ~p~n", [StationTypeForPackage]),
  <<StationTypeForPackage:1/binary,
    DataForPackage:24/binary,
    Slot:8/integer,
    Timestamp:64/integer-big>>.


createTimer(WaitTime, Msg) when WaitTime < 0 ->
  createTimer(0, Msg);
createTimer(WaitTime, Msg) ->
  erlang:send_after(WaitTime, self(), Msg).

cancelTimer(timer) ->
  ok;
cancelTimer(Timer) ->
  erlang:cancel_timer(Timer).