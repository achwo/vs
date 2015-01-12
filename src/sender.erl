-module(sender).
-export([start/0]).

start() -> spawn(fun() -> loop() end).

loop() ->
  receive 
    {data, Data} -> 
      data(Data),
      loop();
    {new_timer, WaitTime} -> 
      newTimer(WaitTime),
      loop();
    {send} ->
      send(),
      loop()
  end,
  loop().


data(Data) ->
  % remember for later use
  todo.

newTimer(WaitTime) ->
  % start new Timer, which sends {send} to self
  todo.

send() ->
  % send msg via multicast
  todo.