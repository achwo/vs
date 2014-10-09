-module(client).
-export([send/1]).

send(Server) -> 
	Server ! {getmessages, self()},
	receive
		{reply, Number, Nachricht, Terminated} ->
			Nachricht	
	end.
