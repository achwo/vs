-module(deliveryqueue).
-export([add/3, get/2, get_max_number/0]).
-import(werkzeug, [findSL/2]).

add(Content, ID, Queue) ->
	%TODO: is_full
	case is_full(Queue)

		true -> 
			%TODO: fun for find the lowest ID
			lists:delete({_,Lowest_ID}, Queue);
			lists:append(Queue, [{Content,ID}]);
	
		false ->
			 lists:append(Queue, [{Content,ID}]);
	end 

get(ID, Queue) ->

	%Message = {Content, ID}	
	Message = findSL(Queue,ID);

	case Message == {-1,nok} 
		true -> 
			[Message, true];
		false -> 
			[Message, false];
	end




