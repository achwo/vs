-module(client).
-export([send/1]).

send(Server) -> 
	Server ! {self(), 'hello'},
	receive
		{Server, ok} ->
			erfolgreich
end.
