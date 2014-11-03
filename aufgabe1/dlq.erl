-module(dlq).
-export([createNew/0, add/3, get/2, getLastMsgNr/1, getLowestMsgNr/1]).

%nachrichtenformat: [{Nachricht, NewNumber}]

createNew() -> [].

% speichert die Nachtricht in die dql
add(Message, Number, Queue) ->
	%Eingang in die dlq wird dokumentiert
	New_Message = lists:concat([Message, "; DLQ In: ",werkzeug:timeMilliSecond()]),

	case is_full(Queue) of 
		true 	-> NewQueue = lists:append(delete_lowest_id(Queue), [{New_Message, Number}]);
		false -> NewQueue = lists:append(Queue, [{New_Message, Number}])
	end,
	lists:keysort(2, NewQueue).

get(_, []) -> false;
get(Number, [{_, NewNumber}|Rest]) when NewNumber /= Number -> get(Number, Rest);
get(_, [{Message, NewNumber}|Rest]) when Rest /= [] -> {Message, NewNumber, true};
get(_, [{Message, NewNumber}|_]) -> {Message, NewNumber, false}.

getLowestMsgNr([{_, ID}|_]) -> ID.

getLastMsgNr(Queue) -> 
	lists:last(Queue).

is_full(Queue) -> 
	{_, DLQ_max_size} = application:get_env(server, dlqlimit),
	DLQ_max_size == length(Queue).

delete_lowest_id([_|Rest]) -> Rest.

