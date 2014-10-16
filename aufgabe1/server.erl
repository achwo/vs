- module(server).
- import(werkzeug, [get_config_value/2]).
- export([start/0,loop/0]).

start() ->
	{ok, ConfigListe} = file:consult("server.cfg"),
	{ok, Servername} = get_config_value(servername, ConfigListe),
	PID = spawn (server,loop,[]),
	register(Servername, PID),
	PID.

loop() -> receive 
	{getmessages, Client} ->
           Client ! {reply, 2, nachricht, true};
    
    {getmsgid,Client} ->
         Client ! {nid, 2}

          end,
loop.

