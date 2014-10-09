- module(server).
- export([start/0,loop/0]).

start() -> spawn(server,loop,[]).

loop() -> receive 
	{getmessages, Client} ->
           Client ! {reply, number, nachricht, true}
          end,
loop.

