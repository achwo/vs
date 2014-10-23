-module(deliveryqueue).
-export([add/3, get/2, get_max_number/1,is_full/2,delete_lowest_id/1]).

add(Content, ID, Queue) ->
	
	IsFull = is_full(Queue,todo),
	if  IsFull -> 
		delete_lowest_id(Queue)
	end,
	lists:append(Queue, [{ID,Content}]).	

get(ID, Queue) -> 
	TerminatedFlag = ID >= get_max_number(Queue),
	{{Queue,ID},TerminatedFlag}.
		

get_max_number(Queue) -> 
	{ID,_} = lists:max(Queue),
	ID.

is_full(Queue, Size) -> 
	length(Queue) == Size.

delete_lowest_id(Queue) -> 
	Min = lists:min(Queue),
	lists:delete(Min,Queue).

