-module(ggt).
-import(werkzeug,[logging/2, to_String/1, get_config_value/2, timeMilliSecond/0]).
-export([start/8]).

%{setneighbors,LeftN,RightN}: die (lokal auf deren Node registrieten und im Namensdienst registriexrten) Namen des linken und rechten Nachbarn werden gesetzt.
%{setpm,MiNeu}: die von diesem Prozess zu berabeitenden Zahl für eine neue Berechnung wird gesetzt.
%{sendy,Y}: der rekursive Aufruf der ggT Berechnung.
%{abstimmung,Initiator}: Wahlnachricht für die Terminierung der aktuellen Berechnung; Initiator ist der Initiator dieser Wahl (z.B. Name des ggT-Prozesses).
%{tellmi,From}: Sendet das aktuelle Mi an From: From ! {mi,Mi}. Wird vom Koordinator z.B. genutzt, um bei einem Berechnungsstillstand die Mi-Situation im Ring anzuzeigen.
%{pingGGT,From}: Sendet ein pongGGT an From: From ! {pongGGT,GGTname}. Wird vom Koordinator z.B. genutzt, um auf manuelle Anforderung hin die Lebendigkeit des Rings zu prüfen.
%kill: der ggT-Prozess wird beendet.

start(StarterId, GGTProzessZahl, Arbeitszeit, TermZeit, Nameservice, Koordinator, Praktikumsgruppe, Teamnummer) ->
  
  LogFile = lists:concat(["GGTP_", to_String(node()), ".log"]),
  StartLog = lists:concat([Praktikumsgruppe, Teamnummer, GGTProzessZahl, StarterId, "Startzeit:", timeMilliSecond(), "mit ", to_String(node()), "auf ", to_String(self()), " \n"]),
  logging(LogFile, StartLog),

  GgtName = buildName(Praktikumsgruppe, Teamnummer, GGTProzessZahl),
  logging(LogFile, lists:concat(["Build Ggt-Name: ", to_String(GgtName)])),
  register(GgtName,self()),
  
  Koordinator ! {hello, GgtName},
  logging(LogFile, lists:concat(["Beim Koordinator ", to_String(Koordinator), " gemeldet."])),
  

  
  % todo: geht nicht?
  Nameservice ! {self(),{bind,GgtName,node()}},
  receive ok -> io:format("..bind.done.\n");
    in_use -> io:format("..schon gebunden.\n")
  end,

loop(Nameservice, GgtName).





loop(Nameservice, GgtName) ->
  receive 
    {kill} -> die(Nameservice, GgtName);
    _ -> loop(Nameservice, GgtName)
  end.



die(Nameservice, GgtName) ->
  Nameservice ! {self(),{unbind,GgtName}},
  receive 
    ok -> io:format("..unbind..done.\n")
  end,
  unregister(GgtName).

buildName(Praktikumsgruppe, Teamnummer, GGTProzessZahl) ->
 erlang:list_to_atom(lists:concat([Praktikumsgruppe, Teamnummer, GGTProzessZahl, 1])).
  



