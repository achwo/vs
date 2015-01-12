-module(data_sink).
-export([start/0]).

start() -> spawn(fun() -> loop() end).

loop() ->
  receive 
    {data, Data} -> data(Data)
  end,
  loop().

data(Data) ->
  io:fwrite("~p~n", [Data]). % todo: use output file