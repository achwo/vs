-module(data_source).
-export([start/0]).

start() ->
  spawn(fun() -> init() end).

init() ->
  receive
  	{setListener, Receiver} -> loop(Receiver)
  end.

loop(Receiver) ->
  Message = io:get_chars("", 24),
  Receiver ! {data, Message},
  loop(Receiver).