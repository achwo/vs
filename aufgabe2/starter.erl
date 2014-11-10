%{steeringval,ArbeitsZeit,TermZeit,GGTProzessnummer}: 
%die steuernden Werte für die ggT-Prozesse werden im Starter Prozess gesetzt; 
%Arbeitszeit ist die simulierte Verzögerungszeit zur Berechnung in Sekunden, 
%TermZeit ist die Wartezeit in Sekunden, bis eine Wahl für eine Terminierung initiiert wird 
%und GGTProzessnummer ist die Anzahl der zu startenden ggT-Prozesse.
-module(starter).
-import(werkzeug, [get_config_value/2, logging/2, timeMilliSecond/0, to_String/1]).
-export([start/2, startGGT/8]).
%Starter_11-ggTs@Brummpa-KLC Startzeit: 01.12 15:49:57,839| mit PID <0.37.0>

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


start(UniqueID, Koordinator) -> 
    LogFile = lists:concat(["Starter_", UniqueID, " ", to_String(node()),".log"]),
    StartLog = lists:concat(["Starter_", UniqueID, "-", to_String(node()), " Startzeit: ", timeMilliSecond(), "mit PID ", to_String(self()), " \n"]),
    logging(LogFile, StartLog),
    load_config(),

    logging(LogFile, lists:concat(["ggt.cfg gelesen..."])),
    Koordinator ! {getsteeringval, self()},
    RegisterKoordinatorLog = lists:concat(["Koordinator ", to_String(Koordinator)," gebunden \n"]),
    logging(LogFile, RegisterKoordinatorLog),
    
    
    receive
      {steeringval, Arbeitszeit, TermZeit, GGTProzessAnzahl} -> 
      ReceiveSteeringValLog = lists:concat(["getsteeringval: ", Arbeitszeit, " Arbeitszeit ggT; ", TermZeit, " Wartezeit ggT, ", GGTProzessAnzahl, " Anzahl GGT Prozesse. \n"]),
      logging(LogFile, ReceiveSteeringValLog),
      nix
    end,
    % Arbeitszeit = 5,
    % TermZeit = 5,
    % Koordinator = 4,
     GGTProzessAnzahl2 = 2,
    
    Nameservice = findNameService(),
    RegisterNameserviceLog = lists:concat(["Nameservice ", Nameservice, "gebunden...\n"]),
    logging(LogFile, RegisterNameserviceLog),
    

    startGGT(UniqueID, GGTProzessAnzahl2, Arbeitszeit, TermZeit, Nameservice, Koordinator, config(praktikumsgruppe), config(teamnummer)).



findNameService() ->
  NameserviceNode = config(nameservicenode),
  net_adm:ping(NameserviceNode),
  timer:sleep(1000),
  NS = global:whereis_name(nameservice),
  io:fwrite("NS: ~p~n",[NS]).

startGGT(_, 0, _, _, _, _, _, _) -> nix;
startGGT(UniqueID, GGTProzessAnzahl, Arbeitszeit, TermZeit, Nameservice, Koordinator, Praktikumsgruppe, Teamnummer) ->
  io:fwrite("startGGT(~p)~n", [GGTProzessAnzahl]),
  spawn_link(fun() -> ggt:start(UniqueID, GGTProzessAnzahl, Arbeitszeit, TermZeit, Nameservice, Koordinator, Praktikumsgruppe, Teamnummer) end),
  startGGT(UniqueID, GGTProzessAnzahl-1, Arbeitszeit, TermZeit, Nameservice, Koordinator, Praktikumsgruppe, Teamnummer).


