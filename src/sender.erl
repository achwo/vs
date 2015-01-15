-module(sender).
-export([start/6]).
-define(DELAY_TOLERANCE_IN_MS, 20).

start(SyncManager, SlotManager, Interface, MultiIP, Port, StationType) ->
  spawn(fun() -> loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, data, timer, sendTime) end).

loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, Data, Timer, SendTime) ->
  io:format("Sender: PID: ~p~n", [self()]),
  receive 
    {data, Data} -> 
    io:format("sender:data~n", []),
      NewData = data(Data),
      loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, NewData, Timer, SendTime);
    
    {new_timer, WaitTime} -> 
    io:format("sender:new_timer~n", []),
      cancelTimer(Timer),
      NewTimer = createTimer(WaitTime, {send}),
      NewSendTime = util:currentTime(SyncManager) + WaitTime,
      loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, Data, NewTimer, NewSendTime);
    
    {reserved_slot, Slot} ->
    io:format("sender:reserved_slot:~p~n", [Slot]),
      CurrentTime = util:currentTime(SyncManager),
      send(CurrentTime, SendTime, Interface, Port, Data, StationType, SyncManager, Slot, MultiIP, SlotManager);
      
    {send} ->
      io:format("sender:send~n", []),
      requestSlot(SlotManager),
      loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, Data, Timer, SendTime);

    MSG -> 
      io:format("sender: unknown message: ~p~n", [MSG]),
      loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, Data, Timer, SendTime)
  end.

data(Data) ->
  Data.

requestSlot(SlotManager) ->
  SlotManager ! {self(), reserve_slot}.



send(CurrentTime, SendTime, Interface, Port, Data, StationType, SyncManager, Slot, MultiIP, SlotManager)
when CurrentTime < abs(SendTime) + ?DELAY_TOLERANCE_IN_MS ->
  io:format("sender:send() Data: ~p~n", [Data]),
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
  Timestamp = sync_util:current_time(SyncManager),
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