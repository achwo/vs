- module(server).
- export([start/0,loop/0]).

start() -> spawn(server,loop,[]).

loop() -> receive 
	{getmessages, Client} ->
           Client ! {reply, 2, nachricht, true};
    
    {getmsgid,Client} ->
         Client ! {nid, 2}

          end,
loop.

