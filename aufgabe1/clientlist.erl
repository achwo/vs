- module(clientlist).
- import(werkzeug, [findSL/2, timeMilliSecond/0]).
- export([createNew/0, add/3, exists/2, update/2, setTime/3, lastMessageID/2, setLastMessageID/3, getMessage/2]).

% format: [{ClientID, LastNumber, TimeStamp}]

createNew() -> [].

add(ID, CurrentTime, Queue) ->
  lists:append(Queue, [{ID, 0, CurrentTime}]).

exists(_, []) -> false;
exists(ID, [{CurrentElement, _, _}|Rest]) when ID /= CurrentElement -> exists(ID, Rest);
exists(ID, [{CurrentElement, _, _}|_]) when ID == CurrentElement -> true.

update(CurrentTime, Queue) -> 
% TODO update(CurrentTime, Queue) -> ClientList: laeuft durch die liste und loescht alte clients (anhand timestamp und config value)
  todo.

setTime(ID, CurrentTime, Queue) ->
% TODO setTime(ID, CurrentTime, Queue) -> ClientList
  case exists(ID, Queue) == true of 
  	true -> {GetID, Number, TimeStamp} = getMessage(ID, Queue),
  			NewList = lists:delete({GetID, Number, TimeStamp}, Queue),
  			add(GetID, CurrentTime, NewList);

  	   _ -> Queue
end.


getMessage(_, []) -> false;
getMessage(ID, [{NewID, _, _}|Rest]) when ID /= NewID -> getMessage(ID, Rest);
getMessage(ID, [{ID, Number, TimeStamp}|_]) -> {ID, Number, TimeStamp}. 


lastMessageID(ID, Queue) -> 
	{_,Number,_} = getMessage(ID, Queue),
	Number.


setLastMessageID(ID, NewMessageID, Queue) ->
% TODO setLastMessageID(ID, NewMessageID, Queue) -> clientlist
  Message = getMessage(ID, Queue),
  NewList = lists:delete()


