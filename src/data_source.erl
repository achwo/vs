-module(data_source).
-export([start/0]).

start() ->
  spawn(fun() -> init() end).

init() ->
  receive
  	{set_listener, Receiver} -> 
      io:format("ds: set_listener~n", []),
      loop(Receiver)
  end.

loop(Receiver) ->
  Data = io:get_chars("", 24),
  io:format("ds: sendData~n", []),
  Receiver ! {data, Data},
  loop(Receiver).