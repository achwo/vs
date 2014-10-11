-module(id).
-export([start/0,get_id/1]).

-record (context, {next_id}).

create_context()->
	#context {next_id = 1}.	

start() ->
	Context = create_context().

get_id(Context) ->
	New_Context = Context#context {
		next_id = Context#context.next_id +1
		},
	%return
	New_Context. 
		
