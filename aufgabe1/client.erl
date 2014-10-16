- module(client).
- import(werkzeug, [to_String/1, timeMilliSecond/0, get_config_value/2, logging/2]).
- export([start/1]).
- compile(export_all).


start(Hostadress) ->
  {ok, ConfigListe} = file:consult("client.cfg"),
  {ok, Servername} = get_config_value(servername, ConfigListe),
  PID = get_PID(Servername, Hostadress),
  

  %TODO: CLIENTNR in LOG-Name ergänzen
  Logfile = lists:concat(["client_", to_String(node()), ".log"]),
  logging(Logfile, to_String(PID)),
  
  Uid = get_unique_id(PID),
  io:fwrite ("Uid ~p~n", [Uid]),
  Message_text = message_builder(Uid, Logfile),
  dropmessage(Servername, Message_text, Uid).
  %TODO: wie komme ich am besten an die Number? Bekomme ich so die MSG?
 % dropmessage(PID, Msg, Number).


get_PID(Servername, Hostadress) ->
	{Servername, list_to_atom(Hostadress)}.

ping_server(Hostname, Adress) ->
	net_adm:ping(erlang:list_to_atom(lists:concat([Hostname,"@", Adress]))).


%TODO: Hier fehlt noch die Anzahl der Nachrichten
message_builder(Message, Logfile) ->
	Msg = lists:concat([to_String(node()), "-", to_String(self()), "-C-1-01:", Message,"te Nachricht. Sendezeit: ", timeMilliSecond(), "(", Message, ")\n"]),
	logging(Logfile, Msg),
	Msg.

dropmessage(Server, Message, Number) ->
	Server ! {dropmessage, {Message, Number}}.

get_unique_id(Server) ->
	io:fwrite ("Config ~p~n", [Server]),
	Server ! {getmsgid, self()},
	io:fwrite ("\nSend Message"),
	receive{nid, Number} ->
		io:fwrite ("Number: ~p~n", [Number]),
		Number
	end.

