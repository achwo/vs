-module(slot_chooser).
-export([start/0]).

start() -> spawn(fun() -> loop() end).

loop() ->
  receive 
    _ -> io:fwrite("Unknown message")
  end,
  loop().