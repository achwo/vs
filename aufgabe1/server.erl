-module(server).
-export([start/0]).

start() ->
  Server = nachrichtendienst:start(),
  server_timer:start(Server, 1000).