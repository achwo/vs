-module(dlq).
-export([createNew/0, add/3, get/2, getLastMsgNr/1, getLowestMsgNr/1]).


% Seite 4 3.4.1.1 -> createNew() :: void -> DLQ
%Ein neues DLQ Objekt wird erstellt und zurückgegeben. Agiert wie ein Konstruktor.
createNew() -> [].

% Seite 4 3.4.1.1 -> add (Msg, Nr, Queue) :: Nachricht x Nummer x DLQ -> DLQ
% Die Nachricht wird der Queue hinten zugefügt, sodass sie sortiert bleibt. 
% Wenn die Queue voll ist und eine weitere Nachricht kommt rein, wird die älteste Nachricht gelöscht, 
% damit die Deliveryqueuesize nie überschritten wird.
add(Message, Number, Queue) ->
	%Eingang in die dlq wird dokumentiert
	New_Message = lists:concat([Message, "; DLQ In: ",werkzeug:timeMilliSecond()]),

	case is_full(Queue) of 
		true 	-> NewQueue = lists:append(delete_lowest_id(Queue), [{New_Message, Number}]);
		false -> NewQueue = lists:append(Queue, [{New_Message, Number}])
	end,
	lists:keysort(2, NewQueue).


% Seite 5 3.4.1.1 -> get (Nr, Queue) :: Nummer x DLQ -> (Nachricht, Nummer , flag)
% Gibt die Nachricht mit der angeforderten Nummer Nr zurück. Die Nummer ist die der erfolgreich angeforderten Nachricht. 
% Das Atom „flag“ gibt an ob noch weitere Nachrichten existieren.
get(_, []) -> false;
get(Number, [{_, NewNumber}|Rest]) when NewNumber /= Number -> get(Number, Rest);
get(_, [{Message, NewNumber}|Rest]) when Rest /= [] -> {Message, NewNumber, false};
get(_, [{Message, NewNumber}|_]) -> {Message, NewNumber, true}.

getLowestMsgNr([]) -> 0;
getLowestMsgNr([{_, ID}|_]) -> ID.

% Seite 5 3.4.1.1 -> getLastMsgNr(DLQueue) :: DlQueue -> Nr
% Gibt die Nummer der letzten Nachricht zurück die in der Deliveryqueue liegen.
getLastMsgNr(Queue) -> 
	lists:last(Queue).

% is_full(Queue) :: Queue -> Boolean
is_full(Queue) -> 
	{_, DLQ_max_size} = application:get_env(server, dlqlimit),
	DLQ_max_size == length(Queue).

% delete_lowest_id(Queue) :: Queue -> DLQ
delete_lowest_id([_|Rest]) -> Rest.

