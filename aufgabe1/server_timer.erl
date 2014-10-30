-module(server_timer).
-export([start/2]).

start(Server, WaitingTimeInMS) ->
  receive
    {ping} -> start(Server, WaitingTimeInMS)
  after WaitingTimeInMS ->
    Server ! {shutdown}
  end.