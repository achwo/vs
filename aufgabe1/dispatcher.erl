- module(dispatcher).
- import(werkzeug, [get_config_value/2, timeMilliSecond/0, to_String/1, logging/2]).
- export([start/1]).

% TODO logging

start(Timer) ->
  load_config(),
  {_, ServerName} = application:get_env(server, servername),
	Logfile = lists:concat(["dispatcher_", to_String(node()), ".log"]),
  Startlog = lists:concat(["Server Startzeit: ", timeMilliSecond(),"mit PID ", to_String(node())]),
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
	receive 
    {getmessages, Client} ->
      New_ID = ID,
      New_HBQ = HBQ,
      New_DLQ = DLQ,
      % pruefen, welche nachricht der client bekommen soll, falls er schon bekannt ist
      clientlist:add(ID,timeMilliSecond(),Clientlist),
      ClientListNumber = clientlist:lastMessageID(Client, Clientlist),
      if 
        % sonst hole kleinste nachricht
        ClientListNumber == 0 -> Number = 1;
        % wenn bekannt, hole nachricht > letzter erhaltener
        true -> Number = ClientListNumber + 1
      end,

      DlqMessage = dlq:get(Number, DLQ),

      case DlqMessage of
        false -> {Message, ActualNumber, MoreMessages} = {"empty list",-1,false};
        _ -> {Message, ActualNumber, MoreMessages} = DlqMessage
      end,

      % todo: what if there is an error? currently: message {reply, nil, nok, true}      
      Client ! {reply, ActualNumber, Message, MoreMessages};
      %MsgToServerLog = lists:concat(["Server: Nachrichtennummer ", ActualNumber, " an ", Client, " gesendet~n"]),
      %logging(Logfile, MsgToServerLog);

    {getmsgid,Client} ->
      New_ID = get_next_id(ID),
      New_HBQ = HBQ,
      New_DLQ = DLQ,
      Client ! {nid, New_ID},
      GetMsgIDLog = lists:concat(["Client bekommt folgende Nummer: ", New_ID]), 
      logging(Logfile, GetMsgIDLog);

    {dropmessage, {Message, Number}} -> 
      % TODO dropmessage: falsche nummern abfangen
      DropmessageLog = lists:concat(["---------------------Aufruf von dropmessage---------------------~n"]),
      logging(Logfile, DropmessageLog),
      New_ID = ID,
      {New_HBQ, New_DLQ} = hbq:add(Message, Number, HBQ, DLQ),
      MessageLog = lists:concat(["Nachricht ", Message , " ", Number, " in HBQ gespeichert~n"]),
      logging(Logfile, MessageLog);

    {shutdown} ->
      io:fwrite("#################SERVER WIRD HERUNTERGEFAHREN#################"),
      init:stop(1),
      New_ID = ID,
      New_HBQ = HBQ,
      New_DLQ = DLQ
  end,

Timer ! {ping},
loop(New_ID, New_DLQ, New_HBQ, Clientlist, Logfile, Timer).

get_next_id(ID) ->
	ID + 1.



	
