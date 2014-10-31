- module(clientlist).
- export([createNew/0, add/3, exists/2, update/2, setTime/3, lastMessageID/2, setLastMessageID/3, getMessage/2]).

% format: [{ClientID, LastNumber, TimeStamp}]

createNew() -> [].

add(ClientID, CurrentTime, Queue) ->
  case exists(ClientID, Queue) of
    true -> setTime(ClientID, CurrentTime, Queue);
    false -> lists:append(Queue, [{ClientID, 0, CurrentTime}])
  end.


exists(_, []) -> false;
exists(ClientID, [{CurrentElement, _, _}|Rest]) when ClientID /= CurrentElement -> exists(ClientID, Rest);
exists(ClientID, [{CurrentElement, _, _}|_]) when ClientID == CurrentElement -> true.

update(CurrentTime, Queue) -> 
{_, LifetimeInS} = application:get_env(server, clientlifetime), 
update(CurrentTime, Queue, [], LifetimeInS).
  

update(_, [], ClientList, _) -> ClientList;

update(CurrentTime, [{_,_,TimeStamp}|Rest], ClientList,LifetimeInS) when (CurrentTime - TimeStamp) > (LifetimeInS*1000) ->
	update(CurrentTime, Rest, ClientList, LifetimeInS);

update(CurrentTime, [{ClientID,LastNumber,TimeStamp}|Rest], ClientList, LifetimeInS) when (CurrentTime - TimeStamp) =< (LifetimeInS*1000) ->
 	NewList = lists:append([{ClientID,LastNumber,TimeStamp}], ClientList),
 	update(CurrentTime, Rest, NewList, LifetimeInS).


setTime(ClientID, CurrentTime, Queue) ->

  case exists(ClientID, Queue) of 
  	true -> {GetID, Number, TimeStamp} = getMessage(ClientID, Queue),
  			NewList = lists:delete({GetID, Number, TimeStamp}, Queue),
  			lists:append(NewList,[{GetID, Number, CurrentTime}]);
  	false -> Queue
end.


getMessage(_, []) -> {0,0,0};
getMessage(ClientID, [{NewClientID, _, _}|Rest]) when ClientID /= NewClientID -> getMessage(ClientID, Rest);
getMessage(ClientID, [{ClientID, Number, TimeStamp}|_]) -> {ClientID, Number, TimeStamp}. 


lastMessageID(ClientID, Queue) -> 
	{_,Number,_} = getMessage(ClientID, Queue),
	Number.


setLastMessageID(ClientID, NewMessageID, Queue) ->
  {NewID, Number, TimeStamp} = getMessage(ClientID, Queue),
  NewList = lists:delete({NewID, Number, TimeStamp}, Queue),
  lists:append(NewList,[{NewID,NewMessageID,TimeStamp}]).
  


