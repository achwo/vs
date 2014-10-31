- module(clientlist).
- export([createNew/0, add/3, exists/2, update/2, setTime/3, lastMessageID/2, setLastMessageID/3, getMessage/2]).

% format: [{ClientID, LastNumber, TimeStamp}]

createNew() -> [].

add(ID, CurrentTime, Queue) ->
  lists:append(Queue, [{ID, 0, CurrentTime}]).

exists(_, []) -> false;
exists(ID, [{CurrentElement, _, _}|Rest]) when ID /= CurrentElement -> exists(ID, Rest);
exists(ID, [{CurrentElement, _, _}|_]) when ID == CurrentElement -> true.

update(CurrentTime, Queue) -> 
{_, LifetimeInS} = application:get_env(server, clientlifetime), 
update(CurrentTime, Queue, [], LifetimeInS).
  

update(_, [], ClientList, _) -> ClientList;
update(CurrentTime, [{_,_,TimeStamp}|Rest], ClientList,LifetimeInS) when (CurrentTime - TimeStamp) > (LifetimeInS*1000) ->
	update(CurrentTime, Rest, ClientList, LifetimeInS);
update(CurrentTime, [{ClientID,LastNumber,TimeStamp}|Rest], ClientList, LifetimeInS) when (CurrentTime - TimeStamp) =< (LifetimeInS*1000) ->
 	NewList = lists:append([{ClientID,LastNumber,TimeStamp}], ClientList),
 	update(CurrentTime, Rest, NewList, LifetimeInS).


setTime(ID, CurrentTime, Queue) ->

  case exists(ID, Queue) of 
  	true -> {GetID, Number, TimeStamp} = getMessage(ID, Queue),
  			NewList = lists:delete({GetID, Number, TimeStamp}, Queue),
  			lists:append(NewList,[{GetID,CurrentTime, NewList}]);
  	false -> Queue
end.


getMessage(_, []) -> {0,0,0};
getMessage(ID, [{NewID, _, _}|Rest]) when ID /= NewID -> getMessage(ID, Rest);
getMessage(ID, [{ID, Number, TimeStamp}|_]) -> {ID, Number, TimeStamp}. 


lastMessageID(ID, Queue) -> 
	{_,Number,_} = getMessage(ID, Queue),
	Number.


setLastMessageID(ID, NewMessageID, Queue) ->
  {NewID, Number, TimeStamp} = getMessage(ID, Queue),
  NewList = lists:delete({NewID, Number, TimeStamp}, Queue),
  lists:append(NewList,[{NewID,NewMessageID,TimeStamp}]).
  


