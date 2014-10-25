-module(dlq).
-import(werkzeug, [findneSL/2]).
-export([createNew/0, add/3, get/2, getLastMsgNr/1]).

%nachrichtenformat: [{Nachricht, NewNumber}]

createNew() -> [].

% speichert die Nachtricht in die dql
add(Message, Number, Queue) ->
	case is_full(Queue) of 
		true 	-> NewQueue = lists:append(delete_lowest_id(Queue), [{Message, Number}]);
		false -> NewQueue = lists:append(Queue, [{Message, Number}])
	end,
	lists:keysort(2, NewQueue).

get(_, []) -> false;


get(Number, [{_, NewNumber}|Rest]) when NewNumber /= Number ->
	get(Number, Rest);

get(_, [{Message, NewNumber}|Rest]) when Rest /= [] ->
	{Message, NewNumber, true};

get(_, [{Message, NewNumber}|_]) ->
	{Message, NewNumber, false}.



getLastMsgNr(Queue) -> 
	lists:last(Queue).

is_full(Queue) -> 
	{_, DLQ_max_size} = application:get_env(server, dlq_max_size),
	DLQ_max_size == length(Queue).

delete_lowest_id(Queue) -> 
	Min = lists:min(Queue),
	lists:delete(Min,Queue).

