- module(clientlist).
- export([createNew/0, add/3, exists/2, update/2, setTime/3, lastMessageID/2, setLastMessageID/3, getMessage/2]).

% Seite 6 3.4.3.1 -> createNew() :: void -> ClientList
% Ein neues ClientList Objekt wird erstellt und zurückgegeben. Agiert wie ein Konstruktor.
createNew() -> [].

% Seite 6 3.4.3.1 -> add(ID, CurrentTime, Queue) :: ClientID x TimeStamp x ClientList -> ClientList
% Fügt einen neuen Clienten mit seiner ID, der Nummer der Nachricht die er als letztes angefragt hat und der Zeit zu der er dies tat hinzu.
add(ClientID, CurrentTime, Queue) ->
  case exists(ClientID, Queue) of
    true -> setTime(ClientID, CurrentTime, Queue);
    false -> lists:append(Queue, [{ClientID, 0, CurrentTime}])
  end.

% Seite 6 3.4.3.1 -> exists(ID, Queue) :: ClientID x ClientList -> Boolean
% Fragt die Liste ob es einen bestimmten Clienten schon gibt
exists(_, []) -> false;
exists(ClientID, [{CurrentElement, _, _}|Rest]) when ClientID /= CurrentElement -> exists(ClientID, Rest);
exists(ClientID, [{CurrentElement, _, _}|_]) when ClientID == CurrentElement -> true.

% Seite 6 3.4.3.1 -> update(CurrentTime, Queue) :: TimeStamp x ClientList -> ClientList
% Läuft über die Liste der Clients und prüft ob es Clienten gibt die sich länger als einen gewisser 
% Intervall nicht gemeldet haben und löscht sie aus der Liste.
update(CurrentTime, Queue) -> 
{_, LifetimeInS} = application:get_env(server, clientlifetime), 
update(CurrentTime, Queue, [], LifetimeInS).
  

update(_, [], ClientList, _) -> ClientList;

update(CurrentTime, [{_,_,TimeStamp}|Rest], ClientList,LifetimeInS) when (CurrentTime - TimeStamp) > (LifetimeInS*1000) ->
	update(CurrentTime, Rest, ClientList, LifetimeInS);

update(CurrentTime, [{ClientID,LastNumber,TimeStamp}|Rest], ClientList, LifetimeInS) when (CurrentTime - TimeStamp) =< (LifetimeInS*1000) ->
 	NewList = lists:append([{ClientID,LastNumber,TimeStamp}], ClientList),
 	update(CurrentTime, Rest, NewList, LifetimeInS).

% Seite 6 3.4.3.1 -> setTime(ID, CurrentTime, Queue) :: ClientID x TimeStamp x ClientList -> ClientList
% Erneuert die Zeit die zu einem Clienten gespeichtert wird wenn er sich meldet.
setTime(ClientID, CurrentTime, Queue) ->

  case exists(ClientID, Queue) of 
  	true -> {GetID, Number, TimeStamp} = getMessage(ClientID, Queue),
  			NewList = lists:delete({GetID, Number, TimeStamp}, Queue),
  			lists:append(NewList,[{GetID, Number, CurrentTime}]);
  	false -> Queue
end.

% getMessage(Number, Queue) :: ClientID x ClientList -> {ClientID, MessageID, Timestamp}
getMessage(_, []) -> {0,0,0};
getMessage(ClientID, [{NewClientID, _, _}|Rest]) when ClientID /= NewClientID -> getMessage(ClientID, Rest);
getMessage(ClientID, [{ClientID, Number, TimeStamp}|_]) -> {ClientID, Number, TimeStamp}. 

% lastMessageID(Number, Queue) :: ClientID x ClientList -> MessageNumber
lastMessageID(ClientID, Queue) -> 
	{_,Number,_} = getMessage(ClientID, Queue),
	Number.

% Seite 6 3.4.3.1 -> setLastMessageID(ID, NewMessageID, Queue) :: ClientID x Number x ClientList -> ClientList
% Setzt die letzte erhaltene Nachricht des Clienten mit der ClientID "ID" auf "NewMessageID" 
% und liefert die veränderte ClientList als Ergebnis zurück.
setLastMessageID(ClientID, NewMessageID, Queue) ->
  {NewID, Number, TimeStamp} = getMessage(ClientID, Queue),
  NewList = lists:delete({NewID, Number, TimeStamp}, Queue),
  lists:append(NewList,[{NewID,NewMessageID,TimeStamp}]).
  


