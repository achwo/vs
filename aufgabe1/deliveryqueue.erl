-module(deliveryqueue).
-export([add/4, get/2, get_max_number/1]).

add(Content, ID, Queue, QueueSize) ->
	
	IsFull = is_full(Queue,QueueSize),
	if  IsFull -> 
		delete_lowest_id(Queue)
	end,
	lists:append(Queue, [{ID,Content}]).	

get(_, []) -> nil;
get(ID, Queue) -> 
	TerminatedFlag = ID >= get_max_number(Queue),
	% TODO if id < smallest number, return smallest
	{{Queue,ID},TerminatedFlag}.

get_max_number(Queue) -> 
	{ID,_} = lists:max(Queue),
	ID.

is_full(Queue, Size) -> 
	length(Queue) == Size.

delete_lowest_id(Queue) -> 
	Min = lists:min(Queue),
	lists:delete(Min,Queue).

