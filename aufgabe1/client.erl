- module(client).
- import(werkzeug, [to_String/1, timeMilliSecond/0, get_config_value/2, logging/2]).
%- export([start/2, send/2, message_builder/0, dropmessage/3]).
- compile(export_all).


start(Hostadress) ->
  {ok, ConfigListe} = file:consult("client.cfg"),
  {ok, Servername} = get_config_value(servername, ConfigListe),
  
  PID = get_PID(Servername, Hostadress),
  
  %TODO: CLIENTNR in LOG-Name ergänzen
  Logfile = lists:concat(["client_", to_String(node()), ".log"]),
  logging(Logfile, to_String(PID)),
 % Msg = get_unique_id(PID, Logfile),
  %TODO: wie komme ich am besten an die Number? Bekomme ich so die MSG?
 % dropmessage(PID, Msg, Number).


get_PID(Servername, Hostadress) ->
	{Servername, list_to_atom(Hostadress)}.


send(Server, Msg) -> 
	Server ! {getmessages, self()},
	receive
		{reply, Number, Nachricht, Terminated} ->
			Nachricht	
	end.


ping_server(Hostname, Adress) ->
	net_adm:ping(erlang:list_to_atom(lists:concat([Hostname,"@", Adress]))).


%TODO: Hier fehlt noch die Anzahl der Nachrichten
message_builder(Message, Logfile) ->
	Msg = lists:concat([to_String(node()), "-", to_String(self()), "-C-1-01:", Message,"te Nachricht. Sendezeit: ", timeMilliSecond(), "(", Message, ")\n"]),
	logging(Logfile, Msg),
	Msg.


dropmessage(Server, Message, Number) ->
	Server ! {dropmessage, {Message, Number}}.
	%TODO: Message noch konvertieren???
	%logging(Logfile,Message).


get_unique_id(Server, Logfile) ->
	Server ! {getmsgid, self()},
	receive{nid, Number} ->
		message_builder(Number, Logfile)
	end.

