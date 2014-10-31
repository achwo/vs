- module(client).
- import(werkzeug, [to_String/1, timeMilliSecond/0, get_config_value/2, logging/2]).
- export([start/1]).
- compile(export_all).


start(Hostadress) ->
  load_config(),
  ServerName = config(servername),
  PID = get_PID(ServerName, Hostadress),
 
  Logfile = lists:concat(["client_", to_String(node()), ".log"]),
  Startlog = lists:concat([name(), " Start: ", timeMilliSecond(),".\n"]),
  logging(Logfile, Startlog),
  
  LifeTime = config(lifetime) * 1000,
  Clients = config(clients),
  
  OwnMessages = [],
  %timer:kill_after(LifeTime * 1000 / 45), % das 45 muss weg, ist nur wegen der warning
  timer:kill_after(LifeTime),
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
  
  OwnMsgs = redakteur(5, PID, OwnMessages, Logfile),
  leser(true, OwnMsgs, PID, Logfile),
  loop(PID, OwnMsgs, Logfile).

redakteur(0, PID, OwnMessages, Logfile) ->
  % vergesse, nachricht zu senden 
  Number = get_unique_id(PID),
  logging(Logfile, lists:concat([Number, "te Nachricht um ", timeMilliSecond(), " vergessen zu senden ******\n"])),
  OwnMessages;
redakteur(HowOften, PID, OwnMessages, Logfile) when HowOften > 0 ->
  % warte n sekunden
  timer:sleep(500),
  % hole nachrichtennummer
  Number = get_unique_id(PID),
  % adde nummer zur liste selbstgeschickter nachrichten
  OwnMessagesNew = lists:append(OwnMessages, [Number]),
  % generiere nachricht
  Message = message_builder(Number),
  SendLog = lists:concat([name(), Number,"te_Nachricht. C Out: ", timeMilliSecond(), " gesendet\n"]),
  % sende nachricht
  dropmessage(PID, Message, Number),
  logging(Logfile, SendLog),
  
  redakteur(HowOften-1, PID, OwnMessagesNew, Logfile).

leser(false, OwnMessages, PID, Logfile) -> redakteur(5, PID, OwnMessages, Logfile);

leser(MoreMessages, OwnMessages, PID, Logfile) when MoreMessages == true -> 
  % hole nachricht
  {MoreMessagesFlag,Message} = receive_message(PID),
  %%TerminatedFlag = true, % nur, damit es nicht endlos laeuft im moment :)
  %überprüft ob die Nachricht von Ihm ist
  {Number, TextMessage} = Message,
  TestFunction = fun(X) -> X =:= Number end,
  IsOwn = lists:any(TestFunction,OwnMessages),
  
  case IsOwn of 
    true -> 
    MessageOwn = lists:concat([TextMessage, ",.own Message; C In: ", timeMilliSecond(),"\n"]),
    logging(Logfile, MessageOwn);
    
    false -> 
      MessageForeign = lists:concat([TextMessage, "; C In: ", timeMilliSecond(),"\n"]),
      logging(Logfile, MessageForeign)
    
  end,
  % generiere ausgabe
  % ausgeben
  
  
  %rekursiver Aufruf
  leser(MoreMessagesFlag,OwnMessages,PID,Logfile).



receive_message(Server) ->
  % fragen Server nach Nachrichten
  Server ! {getmessages, self()},
  % hole Nachrichten vom Server ab
  receive
    {reply, Nachricht, Number, MoreMessagesFlag} ->
     % Speichere empfangene Nachrichten in Liste
     NewMessage = {Nachricht,Number}
   
  end,
  {MoreMessagesFlag, NewMessage}.
  

get_PID(Servername, Hostadress) ->
	{Servername, list_to_atom(Hostadress)}.

ping_server(Hostname, Adress) ->
	net_adm:ping(erlang:list_to_atom(lists:concat([Hostname,"@", Adress]))).


message_builder(MessageNumber) ->
	lists:concat([to_String(node()), "-", to_String(self()), "-C-1-01:", MessageNumber,"te Nachricht. Sendezeit: ", timeMilliSecond(), "(", MessageNumber, ")\n"]).
	
dropmessage(Server, Message, Number) ->
	Server ! {dropmessage, {Message, Number}}.

get_unique_id(Server) ->
	Server ! {getmsgid, self()},
	receive{nid, Number} ->
		Number
	end.

