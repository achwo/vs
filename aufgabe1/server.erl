- module(server).
- import(werkzeug, [get_config_value/2]).
- export([start/0]).


start() ->
	{ok, ConfigListe} = file:consult("server.cfg"),
	{ok, Servername} = get_config_value(servername, ConfigListe),
	ID = 1,
	PID = spawn_link(fun() -> loop(ID) end),
	register(Servername, PID),
	PID.

loop(ID) ->
	New_ID = get_next_id(ID), 
	receive 
    {getmessages, Client} ->
      Client ! {reply, New_ID, nachricht, true};
   
    {getmsgid,Client} ->
      Client ! {nid, New_ID}

  end,
loop(New_ID).

get_next_id(ID) ->
	ID + 1.
	
