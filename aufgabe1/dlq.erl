-module(dlq).
-import(werkzeug, [findneSL/2])
-export([add/4, get/2, get_max_number/1]).

add(Content, ID, Queue, QueueSize) ->
	
	IsFull = is_full(Queue,QueueSize),
	if  IsFull -> 
		delete_lowest_id(Queue)
	end,
	lists:append(Queue, [{ID,Content}]).	

get(_, []) -> {{nil}};
get(ID, Queue) -> 
	% TODO if id < smallest number, return smallest
	ReversedList = lists:reverse(Queue),
	Element = werkzeug:findneSL(ReversedList, ID),
	d
	{ElemID, _} = Element,
	TerminatedFlag = ElemID >= get_max_number(Queue),

	{Element,TerminatedFlag}.

get_max_number(Queue) -> 
	{ID,_} = lists:max(Queue),
	ID.

is_full(Queue, Size) -> 
	length(Queue) == Size.

delete_lowest_id(Queue) -> 
	Min = lists:min(Queue),
	lists:delete(Min,Queue).

