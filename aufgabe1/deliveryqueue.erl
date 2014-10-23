-module(deliveryqueue).
-export([add/3, get/2, get_max_number/0]).
-import(werkzeug, [findSL/2,findneSL/2]).

add(Content, ID, Queue) ->
	%TODO: is_full
	case is_full(Queue) of

		true -> 
			%TODO: fun for find the lowest ID
		%	lists:delete({_,Lowest_ID}, Queue),
			lists:append(Queue, [{Content,ID}]);
	
		false ->
			 lists:append(Queue, [{Content,ID}])
	end. 

get(ID, Queue) -> 
	TerminatedFlag = ID >= get_max_number(),
	{findneSL(Queue,ID),TerminatedFlag}.
		

get_max_number() -> todo.

is_full(Queue) -> todo.


