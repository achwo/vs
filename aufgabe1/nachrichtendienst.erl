- module(nachrichtendienst).
- import(werkzeug, [get_config_value/2]).
- import(dlq, [get/2]).
- export([start/0]).

% TODO clientlist machen, format: [{ClientID, LastNumber, TimeStamp}]
% TODO createNew() :: void -> ClientList
% TODO add(ID, CurrentTime, Queue) :: -> ClientList
% TODO exists(ID, Queue) -> Boolean
% TODO update(CurrentTime, Queue) -> ClientList: laeuft durch die liste und loescht alte clients 
% TODO setTime(ID, CurrentTime, Queue) -> ClientList
% TODO lastMessageID(ClientID, Queue) -> Number
% TODO setLastMessageID(ID, NewMessageID, Queue) -> ClientList

% TODO Server ! {shutdown}
% TODO message: Server ! {ping} ?
% TODO dropmessage: falsche nummern abfangen
% TODO logging



start() ->
	{ok, ConfigListe} = file:consult("server.cfg"),
	{ok, Servername} = get_config_value(servername, ConfigListe),
	ID = 1,
	PID = spawn_link(fun() -> loop(ID, []) end),
	register(Servername, PID),
	PID.

loop(ID, Queue) ->
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

      {{ActualNumber, Message}, Terminated} = dlq:get(Number, Queue),

      % todo: what if there is an error? currently: message {reply, nil, nok, true}      
      Client ! {reply, ActualNumber, Message, Terminated};
   
    {getmsgid,Client} ->
      New_ID = get_next_id(ID), 
      Client ! {nid, New_ID}

  end,
loop(New_ID, Queue).

% returns last received number for Client
client_list_number(Client) ->
  nil.

get_next_id(ID) ->
	ID + 1.
	
