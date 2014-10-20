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
  Startlog = lists:concat([name(), " Start: ", timeMilliSecond(),"."]),
  logging(Logfile, Startlog),
  
  
  {ok, LifeTime} = get_config_value(lifetime, ConfigListe),
  OwnMessages = [],
  %timer:kill_after(LifeTime * 1000 / 45), % das 45 muss weg, ist nur wegen der warning
  timer:kill_after(3000),
  %redakteur(1, PID, OwnMessages, Logfile),
  loop(PID, OwnMessages, Logfile).

  %Uid = get_unique_id(PID),
  %io:fwrite ("Uid ~p~n", [Uid]),
  %Message_text = message_builder(Uid, Logfile),
  %dropmessage(Servername, Message_text, Uid). 
  %TODO: wie komme ich am besten an die Number? Bekomme ich so die MSG?
 % dropmessage(PID, Msg, Number).

name() -> lists:concat([to_String(node()), to_String(self())]).


loop(PID, OwnMessages, Logfile) ->
  redakteur(5, PID, OwnMessages, Logfile),
  leser(false, OwnMessages),
  loop(PID, OwnMessages, Logfile).

redakteur(0, PID, _, Logfile) ->
  % vergesse, nachricht zu senden 
  Number = get_unique_id(PID),
  logging(Logfile, lists:concat([Number, "te Nachricht um ", timeMilliSecond(), " vergessen zu senden ******"]));

redakteur(HowOften, PID, OwnMessages, Logfile) when HowOften > 0 ->
  % warte n sekunden
  timer:sleep(500),
  % hole nachrichtennummer
  Number = get_unique_id(PID),
  % adde nummer zur liste selbstgeschickter nachrichten
  OwnMessagesNew = lists:append(OwnMessages, [Number]),
  % generiere nachricht
  Message = message_builder(Number),
  dropmessage(PID, Message, Number),
  logging(Logfile, Message),
  % sende nachricht
  redakteur(HowOften-1, PID, OwnMessagesNew, Logfile).

leser(Terminated, _) when Terminated == true ->
  nix;

leser(Terminated, OwnMessages) when Terminated == false -> 
  % hole nachricht
  TerminatedFlag = true, % nur, damit es nicht endlos laeuft im moment :)
  % pruefe, ob nachricht selbstgeschickt
  % generiere ausgabe
  % ausgeben
  leser(TerminatedFlag, OwnMessages).

receive_message(HowOften) ->
  receive_message(HowOften-1).

get_PID(Servername, Hostadress) ->
	{Servername, list_to_atom(Hostadress)}.

ping_server(Hostname, Adress) ->
	net_adm:ping(erlang:list_to_atom(lists:concat([Hostname,"@", Adress]))).


message_builder(MessageNumber) ->
	lists:concat([to_String(node()), "-", to_String(self()), "-C-1-01:", MessageNumber,"te Nachricht. Sendezeit: ", timeMilliSecond(), "(", MessageNumber, ")\n"]).
	
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

