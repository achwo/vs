- module(clientlist).
- import(werkzeug, [findSL/2]).
- export([createNew/0, add/3, exists/2, update/2, setTime/3, lastMessageID/2, setLastMessageID/3]).

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
  todo.

lastMessageID(ID, Queue) ->
% TODO lastMessageID(ID, Queue) -> Number
  todo.

setLastMessageID(ID, NewMessageID, Queue) ->
% TODO setLastMessageID(ID, NewMessageID, Queue) -> clientlist
  todo.