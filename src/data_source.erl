-module(data_source).
-export([start/0]).

start() ->
  spawn(fun() -> init() end).

init() ->
  loop(receiver). %todo use real receiver


loop(Receiver) ->
  Message = io:get_chars("", 24),
  Receiver ! {msg, Message},
  loop(Receiver).