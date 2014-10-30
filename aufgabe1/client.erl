- module(client).
- import(werkzeug, [to_String/1, timeMilliSecond/0, get_config_value/2, logging/2]).
- export([start/1]).
- compile(export_all).


start(Hostadress) ->
  load_config(),
  ServerName = config(servername),
  PID = get_PID(ServerName, Hostadress),
  

  %TODO: CLIENTNR in LOG-Name ergänzen
  Logfile = lists:concat(["client_", to_String(node()), ".log"]),
  Startlog = lists:concat([name(), " Start: ", timeMilliSecond(),"."]),
  logging(Logfile, Startlog),
  
  LifeTime = config(lifetime),
  Clients = config(clients),
  
  OwnMessages = [],
  %timer:kill_after(LifeTime * 1000 / 45), % das 45 muss weg, ist nur wegen der warning
  timer:kill_after(3000),
  %redakteur(1, PID, OwnMessages, Logfile),
  loop(PID, OwnMessages, Logfile).

  %Uid = get_unique_id(PID),
  
  %Message_text = message_builder(Uid, Logfile),
  %dropmessage(ServerName, Message_text, Uid). 
  %TODO: wie komme ich am besten an die Number? Bekomme ich so die MSG?
 % dropmessage(PID, Msg, Number).

load_config() ->
  {ok, ConfigFile} = file:consult("client.cfg"),
  
  {ok, Clients} = get_config_value(clients, ConfigFile),
  application:set_env(client, clients, Clients),
  
  {ok, LifeTime} = get_config_value(lifetime, ConfigFile),
  application:set_env(client, lifetime, LifeTime),

  {ok, ServerName} = get_config_value(servername, ConfigFile),
  application:set_env(client, servername, ServerName),

  {ok, Sendeintervall} = get_config_value(sendeintervall, ConfigFile),
  application:set_env(client, sendeintervall, Sendeintervall).

config(Key) ->
  {_, Value} = application:get_env(client, Key),
  Value.

name() -> lists:concat([to_String(node()), to_String(self())]).


loop(PID, OwnMessages, Logfile) ->
  redakteur(5, PID, OwnMessages, Logfile),
  leser(false, OwnMessages, PID, Logfile),
  loop(PID, OwnMessages, Logfile).

redakteur(0, PID, _, Logfile) ->
  % vergesse, nachricht zu senden 
  Number = get_unique_id(PID),
  io:fwrite ("Uid ~p~n", [Number]),
  logging(Logfile, lists:concat([Number, "te Nachricht um ", timeMilliSecond(), " vergessen zu senden ******"]));

redakteur(HowOften, PID, OwnMessages, Logfile) when HowOften > 0 ->
  % warte n sekunden
  timer:sleep(500),
  % hole nachrichtennummer
  Number = get_unique_id(PID),
  io:fwrite ("Uid ~p~n", [Number]),
  % adde nummer zur liste selbstgeschickter nachrichten
  OwnMessagesNew = lists:append(OwnMessages, [Number]),
  % generiere nachricht
  Message = message_builder(Number),
  % sende nachricht
  dropmessage(PID, Message, Number),
  logging(Logfile, Message),
  
  redakteur(HowOften-1, PID, OwnMessagesNew, Logfile).

leser(true, OwnMessages, PID, Logfile) -> nix;

leser(Terminated, OwnMessages, PID, Logfile) when Terminated == false -> 
  % hole nachricht
  {TerminatedFlag,Message} = receive_message(PID),
  %%TerminatedFlag = true, % nur, damit es nicht endlos laeuft im moment :)
  %überprüft ob die Nachricht von Ihm ist
  {Number,TextMessage} = Message,
  io:fwrite ("Number: ~p", [Number]), io:fwrite ("TextMessage: ~p~n", [TextMessage]),
  IsOwn = lists:any(Number,OwnMessages),
  io:fwrite ("IsOwn: ~p~n", [IsOwn]),
  if IsOwn == true -> nix;
    %Schreibe Log mit "own Message"
    false -> nix
    %schreibe Log ohne "own Message"
  end,
  % generiere ausgabe
  % ausgeben
  logging(Logfile, TextMessage),
  %rekursiver Aufruf
  leser(TerminatedFlag,OwnMessages,PID,Logfile).



receive_message(Server) ->
  % fragen Server nach Nachrichten
  Server ! {getmessages, self()},
  % hole Nachrichten vom Server ab
  receive
    {reply, Number, Nachricht, Terminated} ->
     % Speichere empfangene Nachrichten in Liste
     NewMessage = {Number,Nachricht}
  end,
  {Terminated, NewMessage}.
  

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

