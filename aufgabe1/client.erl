- module(client).
- import(werkzeug, [to_String/1, timeMilliSecond/0, get_config_value/2, logging/2]).
%- export([start/2, send/2, message_builder/0, dropmessage/3]).
- compile(export_all).


start(Hostname, Adress) ->
  {ok, ConfigListe} = file:consult("client.cfg"),
  {ok, Servername} = get_config_value(servername, ConfigListe),
  PID = get_PID(Servername, Hostname, Adress),
  logging(lists:concat(["clientLog", timeMilliSecond(), ".log"]), to_String(PID)).
  %Connect = spawn(fun() -> starting()),
  %register(client, Connect).


get_PID(Servername, Hostname, Adress) ->
	{Servername, list_to_atom(lists:concat([Hostname, "@", Adress]))}.


send(Server, Msg) -> 
	Server ! {getmessages, self()},
	receive
		{reply, Number, Nachricht, Terminated} ->
			Nachricht	
	end.


ping_server(Hostname, Adress) ->
	net_adm:ping(erlang:list_to_atom(lists:concat([Hostname,"@", Adress]))).


%TODO: Hier fehlt noch die Anzahl der Nachrichten
message_builder() ->
	Massage = lists:concat([to_String(node()), "-", to_String(self()), "-C-1-01:", "Sendezeit: ", timeMilliSecond(), "|\n"]).


dropmessage(Server, Message, Number) ->
	Server ! {dropmessage, {Number, Message}},
	%TODO: Message noch konvertieren???
	logging(Logfile,Message).



 starting() ->
 	message_builder().

