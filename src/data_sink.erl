-module (data_sink).
-export ([start/0]).

-define(LOG_PATH, "logs/").

start() ->
  io:format("data_sink: starting logging process."),
  spawn(fun() -> loop() end).

loop() ->
  receive
    {data, Data} -> 
      data(Data),
      loop()
  end.

data(Data) ->
  Text = io_lib:format("~s\n", [Data]),
  FilePath = ?LOG_PATH ++ lists:sublist(Data, 1, 10) ++ ".log",
  file:write_file(FilePath, [Text], [append]).