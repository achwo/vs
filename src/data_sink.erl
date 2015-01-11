-module(data_sink).
-export([start/0]).

start() -> spawn(fun() -> loop() end).

loop() ->
  receive 
    {messages, ListOfMessages} -> messages(ListOfMessages)
  end,
  loop().

messages(ListOfMessages) ->
  lists:foreach(fun(Message) ->
      io:fwrite("~p~n", [Message]) 
    end, ListOfMessages).