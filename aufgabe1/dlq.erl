-module(dlq).
-import(werkzeug, [findneSL/2]).
-export([createNew/0, add/3, get/2, getLastMsgNr/1]).

% TODO nachrichtenformat: [{Nachricht, Nr}]

createNew() -> [].

add(Message, Number, Queue) ->
	case is_full(Queue) of 
		true 	-> NewQueue = lists:append(delete_lowest_id(Queue), [{Message, Number}]);
		false -> NewQueue = lists:append(Queue, [{Message, Number}])
	end,
	lists:keysort(2, NewQueue).

get(_, []) -> false;
get(Number, Queue) ->
	% TODO suche nach {_, Number}
	% TODO wenn in get keine nachricht mit der angegebenen nummer gefunden wird, wird "false" zurueckgegeben
	Message = fake,
	Number = -1,
	TerminatedFlag = false,
	{Message, Number, TerminatedFlag}.

getLastMsgNr(Queue) -> 
	todo.

is_full(Queue) -> 
	{_, DLQ_max_size} = application:get_env(server, dlq_max_size),
	DLQ_max_size == length(Queue).

delete_lowest_id(Queue) -> 
	Min = lists:min(Queue),
	lists:delete(Min,Queue).

