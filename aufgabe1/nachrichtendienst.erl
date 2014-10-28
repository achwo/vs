- module(nachrichtendienst).
- import(werkzeug, [get_config_value/2]).
- import(dlq, [get/2]).
- import(hbq,[add/4]).
- import(clientlist,[lastMessageID/2]).
- export([start/0]).

% TODO logging
% TODO config as global: application:set_env(server, dlq_max_size, 10).


start() ->
	{ok, ConfigListe} = file:consult("server.cfg"),
	{ok, Servername} = get_config_value(servername, ConfigListe),
	ID = 0,
	PID = spawn_link(fun() -> loop(ID, dlq:createNew(), hbq:createNew()) end),
	register(Servername, PID),
	PID.

loop(ID, DLQ, HBQ) ->
	receive 
    {getmessages, Client} ->
      New_ID = ID,
      New_HBQ = HBQ,
      New_DLQ = DLQ,
      % pruefen, welche nachricht der client bekommen soll, falls er schon bekannt ist
      ClientListNumber = clientlist:lastMessageID(Client, DLQ),
      if 
        % sonst hole kleinste nachricht
        ClientListNumber == 0 -> Number = 1;
        % wenn bekannt, hole nachricht > letzter erhaltener
        true -> Number = ClientListNumber + 1
      end,

      DlqMessage = dlq:get(Number, DLQ),
      case DlqMessage of
      false -> 
      {{ActualNumber, Message},_,Terminated} = {{-1, "empty list"},-1,false};

      _ -> {{ActualNumber, Message},_,Terminated} = DlqMessage
    end,

      % todo: what if there is an error? currently: message {reply, nil, nok, true}      
      Client ! {reply, ActualNumber, Message, Terminated};
   
    {getmsgid,Client} ->
      New_ID = get_next_id(ID),
      New_HBQ = HBQ,
      New_DLQ = DLQ,
      Client ! {nid, New_ID};

    {dropmessage, {Message, Number}} -> 
      % TODO dropmessage: falsche nummern abfangen
      New_ID = ID,
      {New_HBQ, New_DLQ} = hbq:add(Message, Number, HBQ, DLQ);

    {shutdown} ->
      New_ID = ID,
      New_HBQ = HBQ,
      New_DLQ = DLQ,
      todo;

    {ping} ->
      New_ID = ID,
      New_HBQ = HBQ,
      New_DLQ = DLQ,
      todo
  end,

loop(New_ID, New_DLQ, New_HBQ).

get_next_id(ID) ->
	ID + 1.



	
