-module(sender).
-export([start/0]).

start(SyncManager, SlotManager, Interface, MultiIP, Port, StationType) ->
 spawn(fun() -> loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, data, timer) end).

loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, Data, Timer) ->
  receive 
    {data, Data} -> 
     NewData = data(Data),
      loop(SyncManager, SlotManager, Interface, MultiIP, Port, StationType, NewData);
    {new_timer, WaitTime} -> 
      newTimer(WaitTime),
      loop();
    {send} ->
      send(),
      loop()
  end.


data(Data) ->
  Data.



newTimer(WaitTime) ->
  cancel_timer(WaitTime),
  create_timer(WaitTime, {send}).
 

send() ->
  % send msg via multicast
  todo.

create_timer(WaitTime, Msg) when WaitTime < 0 ->
  create_timer(0, Msg);

create_timer(WaitTime, Msg) ->
  erlang:send_after(WaitTime, self(), Msg).

cancel_timer(undefined) ->
  ok;

cancel_timer(Timer) ->
  erlang:cancel_timer(Timer).