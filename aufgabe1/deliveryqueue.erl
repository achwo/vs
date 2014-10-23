-module(deliveryqueue).
-export([add/3, get/2, get_max_number/1]).

add(Content, ID, Queue) ->
	
	IsFull = is_full(Queue),
	if  IsFull -> 
		delete_lowest_id(Queue)
	end,
	lists:append(Queue, [{ID,Content}]).	

get(ID, Queue) -> 
	TerminatedFlag = ID >= get_max_number(Queue),
	{{Queue,ID},TerminatedFlag}.
		

get_max_number(Queue) -> 
	MapAsList = maps:to_list(Queue),
	{ID,_} = lists:max(MapAsList),
	ID.

is_full(Queue) -> todo.

delete_lowest_id(Queue) -> todo.


