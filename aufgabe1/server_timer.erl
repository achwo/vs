-module(server_timer).
-export([start/1]).

start(Server) ->
  %todo content....
  % every n ms: ping_server(Server)
  ping_server(Server),
todo.

ping_server(Server) ->
Server ! {ping},
receive
  {answer} -> Result = true;
  {timeout} -> Result = false
end,
Result.