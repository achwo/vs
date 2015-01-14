-module(data_sink).
-export([start/0]).

start() -> 
	io:format ("------data_sink------"),
	LogFile = lists:concat(["DataSink.log"]),
	spawn(fun() -> loop(LogFile) end).

loop(LogFile) ->
  receive 
    {data, Data} -> data(Data, LogFile)
  end,
  loop(LogFile).

data(Data, LogFile) ->
  io:fwrite("~p~n", [Data]), % todo: use output file
  log(LogFile, Data).

 
 log(LogFile, Message) ->
 werkzeug:logging(LogFile, Message ++ "\n").