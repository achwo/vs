%{steeringval,ArbeitsZeit,TermZeit,GGTProzessnummer}: 
%die steuernden Werte für die ggT-Prozesse werden im Starter Prozess gesetzt; 
%Arbeitszeit ist die simulierte Verzögerungszeit zur Berechnung in Sekunden, 
%TermZeit ist die Wartezeit in Sekunden, bis eine Wahl für eine Terminierung
% initiiert wird 
%und GGTProzessnummer ist die Anzahl der zu startenden ggT-Prozesse.
-module(starter).
-import(werkzeug, [get_config_value/2, timeMilliSecond/0, to_String/1]).
-import(utility, [log/2]).
-export([start/1]).

config(Key) -> utility:from_config(ggt, Key).

start(UniqueID) -> 
  StarterName = lists:concat(["Starter_", UniqueID]),
  NodeString = to_String(node()),

  LogFile = lists:concat([StarterName, " ", NodeString,".log"]),
  log(LogFile, lists:concat([StarterName, "-", NodeString, 
    " Startzeit: ", timeMilliSecond(), "mit PID ", to_String(self())])),
  {ok, ConfigFile} = file:consult("ggt.cfg"),
  utility:load_config(ggt, ConfigFile),
  log(LogFile, "ggt.cfg gelesen..."),

  Nameservice = utility:find_nameservice(
    config(nameservicenode), 
    config(nameservicename)),

  Koordinator = utility:find_process(koordinator, Nameservice),

  Koordinator ! {getsteeringval, self()},
  log(LogFile, 
    lists:concat(["Koordinator ", to_String(Koordinator)," gebunden"])),

  receive
    {steeringval, Arbeitszeit, TermZeit, GGTProzessAnzahl} -> 
      log(LogFile, 
        lists:concat(["getsteeringval: ", Arbeitszeit, " Arbeitszeit ggT; ", 
          TermZeit, " Wartezeit ggT, ", 
          GGTProzessAnzahl, " Anzahl GGT Prozesse."]))
  end,
  


  log(LogFile, 
    lists:concat(["Nameservice ", to_String(Nameservice), "gebunden..."])),
  startGGT(UniqueID, GGTProzessAnzahl, Arbeitszeit, TermZeit, Nameservice, 
    Koordinator, config(praktikumsgruppe), config(teamnummer), LogFile).

startGGT(_, 0, _, _, _, _, _, _, _) -> ok;
startGGT(UniqueID, GGTProzessAnzahl, Arbeitszeit, TermZeit, Nameservice, 
  Koordinator, Praktikumsgruppe, Teamnummer, LogFile) ->
  log(LogFile, lists:concat(["startGGT(", GGTProzessAnzahl, ")"])),
  spawn_link(fun() -> 
    ggt:start(UniqueID, GGTProzessAnzahl, Arbeitszeit, TermZeit, Nameservice, 
      Koordinator, Praktikumsgruppe, Teamnummer) 
  end),
  startGGT(UniqueID, GGTProzessAnzahl-1, Arbeitszeit, TermZeit, Nameservice, 
    Koordinator, Praktikumsgruppe, Teamnummer, LogFile).


