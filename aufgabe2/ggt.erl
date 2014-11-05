-module(ggt).
-export([start/1]).

%{setneighbors,LeftN,RightN}: die (lokal auf deren Node registrieten und im Namensdienst registriexrten) Namen des linken und rechten Nachbarn werden gesetzt.
%{setpm,MiNeu}: die von diesem Prozess zu berabeitenden Zahl für eine neue Berechnung wird gesetzt.
%{sendy,Y}: der rekursive Aufruf der ggT Berechnung.
%{abstimmung,Initiator}: Wahlnachricht für die Terminierung der aktuellen Berechnung; Initiator ist der Initiator dieser Wahl (z.B. Name des ggT-Prozesses).
%{tellmi,From}: Sendet das aktuelle Mi an From: From ! {mi,Mi}. Wird vom Koordinator z.B. genutzt, um bei einem Berechnungsstillstand die Mi-Situation im Ring anzuzeigen.
%{pingGGT,From}: Sendet ein pongGGT an From: From ! {pongGGT,GGTname}. Wird vom Koordinator z.B. genutzt, um auf manuelle Anforderung hin die Lebendigkeit des Rings zu prüfen.
%kill: der ggT-Prozess wird beendet.
start(Nameservice) ->
  register(ggt,self()),
  Nameservice ! {self(),{bind,ggt,node()}},
  receive ok -> io:format("..bind.done.\n");
    in_use -> io:format("..schon gebunden.\n")
  end,
  loop(Nameservice).


loop(Nameservice) ->
  receive 
    {kill} -> die(Nameservice);
    _ -> loop(Nameservice)
  end.



die(Nameservice) ->
  Nameservice ! {self(),{unbind,ggt}},
  receive 
    ok -> io:format("..unbind..done.\n")
  end,
  unregister(ggt).