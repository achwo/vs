-module(data_sink).
-export([start/0]).

start() -> 
	LogFile = "DataSink.log",
	spawn(fun() -> loop(LogFile) end).

loop(LogFile) ->
  receive 
    {data, Data} -> data(Data, LogFile)
  end,
  loop(LogFile).

data(Data, LogFile) ->
  % io:fwrite("~p~n", [Data]),
  log(LogFile, Data).

log(LogFile, Message) ->
  NewMessage = lists:flatten(io_lib:format("~p", [Message])),
  file:write_file(LogFile, io_lib:fwrite("~p~n", [NewMessage]), [append]).