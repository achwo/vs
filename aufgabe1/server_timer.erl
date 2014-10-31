-module(server_timer).
-import(werkzeug, [to_String/1, timeMilliSecond/0, logging/2]).
-export([prepare/1]).

prepare(WaitingTimeInMS) ->
  receive
    {ServerPID} ->
      start(ServerPID, WaitingTimeInMS)
  end.

start(Server, WaitingTimeInMS) ->
  Logfile = lists:concat(["timer.log"]),
  Startlog = lists:concat(["Startzeit: ", timeMilliSecond()]),
  logging(Logfile, Startlog),
  receive
    {ping} -> start(Server, WaitingTimeInMS)
  after WaitingTimeInMS ->
    Server ! {shutdown}
  end.