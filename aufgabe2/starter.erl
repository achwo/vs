%{steeringval,ArbeitsZeit,TermZeit,GGTProzessnummer}: die steuernden Werte für die ggT-Prozesse werden im Starter Prozess gesetzt; 
%Arbeitszeit ist die simulierte Verzögerungszeit zur Berechnung in Sekunden, 
%TermZeit ist die Wartezeit in Sekunden, bis eine Wahl für eine Terminierung initiiert wird 
%und GGTProzessnummer ist die Anzahl der zu startenden ggT-Prozesse.
-module(starter).
-import(werkzeug, [get_config_value/2]).
-export([start/1, startGGT/5]).

start(Koordinator) -> 
    %Koordinator ! {getsteeringval, self()},
    %receive
    %  {steeringval, Arbeitszeit, TermZeit, GGTProzessAnzahl} -> nix
    %end,
    Arbeitszeit = 5,
    TermZeit = 5,
    Koordinator = 4,
    GGTProzessAnzahl = 5,
    load_config(),
    Nameservice = findNameService(),
    startGGT(GGTProzessAnzahl, Arbeitszeit, TermZeit, Nameservice, Koordinator).

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

startGGT(0, _, _, _, _) -> nix;
startGGT(GGTProzessAnzahl, Arbeitszeit, TermZeit, Nameservice, Koordinator) ->
  io:fwrite("startGGT(~p)~n", [GGTProzessAnzahl]),
  startGGT(GGTProzessAnzahl-1, Arbeitszeit, TermZeit, Nameservice, Koordinator).