-module(sender).
-export([start/6]).
-define(DELAY_TOLERANCE_IN_MS, 20).

start(SyncManager, SlotManager, Interface, MultiIP, Port, StationType) ->
 spawn(fun() -> loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, data, timer, sendTime) end).

loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, Data, Timer, SendTime) ->
  receive 
    {data, Data} -> 
     NewData = data(Data),
      loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, NewData, Timer, SendTime);
    
    {new_timer, WaitTime} -> 
    cancel_timer(Timer),
    NewTimer = create_timer(WaitTime, {send}),
    NewSendTime = sync_util:current_time(SyncManager) + WaitTime,
    loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, Data, NewTimer, NewSendTime);
    
    {reservable_slot, Slot} ->
      CurrentTime = sync_util:current_time(SyncManager),
      send(CurrentTime, SendTime, Interface, Port, Data, StationType, SyncManager, Slot, MultiIP, SlotManager);
      
    {send} ->
      requestSlot(SlotManager),
      loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, Data, Timer, SendTime)
  end.


data(Data) ->
  Data.


requestSlot(SlotManager) ->
  SlotManager ! {get_reservable_slot}.


send(CurrentTime, SendTime, Interface, Port, Data, StationType, SyncManager, Slot, MultiIP, _)
when CurrentTime < abs(SendTime) + ?DELAY_TOLERANCE_IN_MS ->
  Socket = werkzeug:openSe(Interface, Port),
  Packet = buildPackage(Data, StationType, SyncManager, Slot),
  ok = gen_udp:send(Socket, MultiIP, Port, Packet);
  send(_, _, _, _, _, _, _, _, _, SlotManager) ->
   SlotManager ! {slot_missed}.
  


buildPackage(Data, StationType, SyncManager, Slot) ->
  DataForPackage = list_to_binary (Data),
  StationTypeForPackage = list_to_binary (StationType),
  Timestamp = sync_util:current_time(SyncManager),
  <<StationTypeForPackage:1/binary,
    DataForPackage:24/binary,
    Slot:8/integer,
    Timestamp:64/integer-big>>.



create_timer(WaitTime, Msg) when WaitTime < 0 ->
  create_timer(0, Msg);
create_timer(WaitTime, Msg) ->
  erlang:send_after(WaitTime, self(), Msg).

cancel_timer(undefined) ->
  ok;
cancel_timer(Timer) ->
  erlang:cancel_timer(Timer).