-module(server_timer).
-export([start/2]).

start(Server, WaitingTimeInMS) ->
  Server ! {ping},

  receive
    {ping} -> start(Server, WaitingTimeInMS)
  after WaitingTimeInMS ->
    Server ! {shutdown}
  end.