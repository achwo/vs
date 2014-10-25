- module(nachrichtendienst).
- import(werkzeug, [get_config_value/2]).
- import(dlq, [get/2]).
- export([start/0]).

% TODO logging
% TODO config as global: application:set_env(server, dlq_max_size, 10).


start() ->
	{ok, ConfigListe} = file:consult("server.cfg"),
	{ok, Servername} = get_config_value(servername, ConfigListe),
	ID = 1,
	PID = spawn_link(fun() -> loop(ID, dlq:createNew(), hbq:createNew()) end),
	register(Servername, PID),
	PID.

loop(ID, DLQ, HBQ) ->
	receive 
    {getmessages, Client} ->
      New_ID = ID,

      % pruefen, welche nachricht der client bekommen soll, falls er schon bekannt ist
      ClientListNumber = client_list_number(Client),
      if 
        % sonst hole kleinste nachricht
        ClientListNumber == nil -> Number = 1;
        % wenn bekannt, hole nachricht > letzter erhaltener
        true -> Number = ClientListNumber + 1
      end,

      {{ActualNumber, Message}, Terminated} = dlq:get(Number, DLQ),

      % todo: what if there is an error? currently: message {reply, nil, nok, true}      
      Client ! {reply, ActualNumber, Message, Terminated};
   
    {getmsgid,Client} ->
      New_ID = get_next_id(ID), 
      Client ! {nid, New_ID};

    {dropmessage, {Message, Number}} -> 
      % TODO dropmessage: falsche nummern abfangen
      New_ID = ID,
      {New_HBQ, New_DLQ} = dropmessage(Message, Number, HBQ, DLQ);

    {shutdown} ->
      New_ID = ID,
      todo;

    {ping} ->
      New_ID = ID,
      todo
  end,

loop(New_ID, DLQ, HBQ).

% returns last received number for Client
client_list_number(Client) ->
  nil.

get_next_id(ID) ->
	ID + 1.

dropmessage(Message, Number, HBQ, DLQ) -> 
  todo.
	
