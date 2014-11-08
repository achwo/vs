%{steeringval,ArbeitsZeit,TermZeit,GGTProzessnummer}: 
%die steuernden Werte für die ggT-Prozesse werden im Starter Prozess gesetzt; 
%Arbeitszeit ist die simulierte Verzögerungszeit zur Berechnung in Sekunden, 
%TermZeit ist die Wartezeit in Sekunden, bis eine Wahl für eine Terminierung initiiert wird 
%und GGTProzessnummer ist die Anzahl der zu startenden ggT-Prozesse.
-module(starter).
-import(werkzeug, [get_config_value/2, logging/2, timeMilliSecond/0, to_String/1]).
-export([start/1, startGGT/7]).

start(Koordinator) -> 
    LogFile = lists:concat(["Starter_", to_String(node()), ".log"]),
    StartLog = lists:concat(["Started at: ", timeMilliSecond(), " \n"]),
    logging(LogFile, StartLog),


    Koordinator ! {getsteeringval, self()},
    RegisterKoordinatorLog = lists:concat(["Register at Koordinator: ", to_String(Koordinator)," \n"]),
    logging(LogFile, RegisterKoordinatorLog),
    
    receive
      {steeringval, Arbeitszeit, TermZeit, GGTProzessAnzahl} -> 
      ReceiveSteeringValLog = lists:concat(["Steeringval: ", Arbeitszeit, ", ", TermZeit, ", ", GGTProzessAnzahl," \n"]),
      logging(LogFile, ReceiveSteeringValLog),
      nix
    end,
    % Arbeitszeit = 5,
    % TermZeit = 5,
    % Koordinator = 4,
    % GGTProzessAnzahl = 5,
    load_config(),
    Nameservice = findNameService(),
    RegisterNameserviceLog = lists:concat(["Registered at Nameservice: ", Nameservice,"\n"]),
    logging(LogFile, RegisterNameserviceLog),
    startGGT(GGTProzessAnzahl, Arbeitszeit, TermZeit, Nameservice, Koordinator, config(praktikumsgruppe), config(teamnummer)).

load_config() ->
  {ok, ConfigFile} = file:consult("ggt.cfg"),
  
  {ok, Praktikumsgruppe} = get_config_value(praktikumsgruppe, ConfigFile),
  application:set_env(ggt, praktikumsgruppe, Praktikumsgruppe),
  
  {ok, Teamnummer} = get_config_value(teamnummer, ConfigFile),
  application:set_env(ggt, teamnummer, Teamnummer),

  {ok, NameserviceNode} = get_config_value(nameservicenode, ConfigFile),
  application:set_env(ggt, nameservicenode, NameserviceNode),

  {ok, NameserviceName} = get_config_value(nameservicename, ConfigFile),
  application:set_env(ggt, nameservicename, NameserviceName),

  {ok, Koordinatorname} = get_config_value(koordinatorname, ConfigFile),
  application:set_env(ggt, koordinatorname, Koordinatorname).

config(Key) ->
  {_, Value} = application:get_env(ggt, Key),
  Value.

findNameService() ->
  NameserviceNode = config(nameservicenode),
  net_adm:ping(NameserviceNode),
  timer:sleep(1000),
  NS = global:whereis_name(nameservice),
  io:fwrite("NS: ~p~n",[NS]).

startGGT(0, _, _, _, _, _, _) -> nix;
startGGT(GGTProzessAnzahl, Arbeitszeit, TermZeit, Nameservice, Koordinator, Praktikumsgruppe, Teamnummer) ->
  io:fwrite("startGGT(~p)~n", [GGTProzessAnzahl]),
  spawn_link(fun() -> ggt:start(GGTProzessAnzahl, Arbeitszeit, TermZeit, Nameservice, Koordinator, Praktikumsgruppe, Teamnummer) end),
  startGGT(GGTProzessAnzahl-1, Arbeitszeit, TermZeit, Nameservice, Koordinator, Praktikumsgruppe, Teamnummer).


