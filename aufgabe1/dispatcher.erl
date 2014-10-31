- module(dispatcher).
- import(werkzeug, [get_config_value/2, timeMilliSecond/0, to_String/1, logging/2]).
- export([start/1]).

% TODO logging

start(Timer) ->
  load_config(),
  {_, ServerName} = application:get_env(server, servername),
	Logfile = lists:concat(["dispatcher_", to_String(node()), ".log"]),
  Startlog = lists:concat(["Server Startzeit: ", timeMilliSecond(),"mit PID ", to_String(node()), "\n"]),
  logging(Logfile, Startlog),
  ID = 0,
	PID = spawn_link(fun() -> loop(ID, dlq:createNew(), hbq:createNew(), clientlist:createNew(), Logfile, Timer) end),
	register(ServerName, PID),
	PID.



load_config() ->
  {ok, ConfigFile} = file:consult("server.cfg"),
  
  {ok, Latency} = get_config_value(latency, ConfigFile),
  application:set_env(server, latency, Latency),
  
  {ok, ClientLifeTime} = get_config_value(clientlifetime, ConfigFile),
  application:set_env(server, clientlifetime, ClientLifeTime),

  {ok, ServerName} = get_config_value(servername, ConfigFile),
  application:set_env(server, servername, ServerName),

  {ok, DLQLimit} = get_config_value(dlqlimit, ConfigFile),
  application:set_env(server, dlqlimit, DLQLimit).


loop(ID, DLQ, HBQ, Clientlist, Logfile, Timer) ->
  Timer ! {ping},
	receive 
    {getmessages, Client} ->
      % pruefen, welche nachricht der client bekommen soll, falls er schon bekannt ist
      ModifiedClientList = clientlist:add(Client,get_timestamp(),Clientlist),
      ClientListNumber = clientlist:lastMessageID(Client, Clientlist),
      if 
        % sonst hole kleinste nachricht
        ClientListNumber == 0 -> Number = 1;
        % wenn bekannt, hole nachricht > letzter erhaltener
        true -> Number = ClientListNumber + 1
      end,

      DlqMessage = dlq:get(Number, DLQ),

      case DlqMessage of
        false -> 
          {Message, ActualNumber, MoreMessages} = {"empty list",-1,false},
          NewModifiedClientList = ModifiedClientList;
        _ -> 
          {Message, ActualNumber, MoreMessages} = DlqMessage,
          NewModifiedClientList = clientlist:setLastMessageID(Client, ActualNumber, ModifiedClientList)
      end,

      GetmessagesLog = lists:concat([Message, "-getmessages von ", to_String(Client), "-", to_String(MoreMessages),"\n"]),
      logging(Logfile, GetmessagesLog),

      Client ! {reply, ActualNumber, Message, MoreMessages},
      loop(ID, DLQ, HBQ, NewModifiedClientList, Logfile, Timer);

    {getmsgid,Client} ->
      New_ID = get_next_id(ID),
      Client ! {nid, New_ID},
      GetMsgIDLog = lists:concat(["Nachrichtennummer ", to_String(New_ID), " an ", to_String(Client), " gesendet\n"]), 
      logging(Logfile, GetMsgIDLog),
      loop(New_ID, DLQ, HBQ, Clientlist, Logfile, Timer);

    {dropmessage, {Message, Number}} -> 
      {New_HBQ, New_DLQ, TransferedNumbers} = hbq:add(Message, Number, HBQ, DLQ),
      MessageLog = lists:concat(["Nachricht ", Message , " ", Number, " in HBQ gespeichert\n"]),
      logging(Logfile, MessageLog),

      case TransferedNumbers == [] of
        false ->
          TransferLog = lists:concat(["QVerwaltung>>> Nachrichten ", to_String(TransferedNumbers), " von HBQ in DLQ transferiert.\n"]),
          logging(Logfile, TransferLog);
        _ -> nothing
      end,  
      loop(ID, New_DLQ, New_HBQ, Clientlist, Logfile, Timer);

    {shutdown} ->
      io:fwrite("#################SERVER WIRD HERUNTERGEFAHREN#################\n"),
      MessageLog = lists:concat(["Downtime ", timeMilliSecond(), " vom Nachrichtenserver ", to_String(self()), 
        "; Anzahl der Restnachrichten in der HBQ:",to_String(length(HBQ)), "\n"]),
      logging(Logfile, MessageLog)
  end,
  init:stop(1).

get_next_id(ID) ->
	ID + 1.

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).


	
