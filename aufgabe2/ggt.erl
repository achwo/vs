-module(ggt).
-import(werkzeug,[logging/2, to_String/1, get_config_value/2, timeMilliSecond/0]).
-export([start/8, calculateMi/2]).

%{setneighbors,LeftN,RightN}: die (lokal auf deren Node registrieten und im Namensdienst registriexrten) Namen des linken und rechten Nachbarn werden gesetzt.
%{setpm,MiNeu}: die von diesem Prozess zu berabeitenden Zahl für eine neue Berechnung wird gesetzt.
%{sendy,Y}: der rekursive Aufruf der ggT Berechnung.
%{abstimmung,Initiator}: Wahlnachricht für die Terminierung der aktuellen Berechnung; Initiator ist der Initiator dieser Wahl (z.B. Name des ggT-Prozesses).
%{tellmi,From}: Sendet das aktuelle Mi an From: From ! {mi,Mi}. Wird vom Koordinator z.B. genutzt, um bei einem Berechnungsstillstand die Mi-Situation im Ring anzuzeigen.
%{pingGGT,From}: Sendet ein pongGGT an From: From ! {pongGGT,GGTname}. Wird vom Koordinator z.B. genutzt, um auf manuelle Anforderung hin die Lebendigkeit des Rings zu prüfen.
%kill: der ggT-Prozess wird beendet.

start(StarterId, GGTProzessZahl, Arbeitszeit, TermZeit, Nameservice, Koordinator, Praktikumsgruppe, Teamnummer) ->
  
  LogFile = lists:concat(["GGTP_", to_String(node()), ".log"]),
  StartLog = lists:concat([Praktikumsgruppe, Teamnummer, GGTProzessZahl, StarterId, " Startzeit:", timeMilliSecond(), "mit ", to_String(node()), "auf ", to_String(self()), " \n"]),
  logging(LogFile, StartLog),

  GgtName = buildName(Praktikumsgruppe, Teamnummer, GGTProzessZahl),
  logging(LogFile, lists:concat(["Build Ggt-Name: ", to_String(GgtName), "\n"])),
  global:register_name(GgtName,self()),
  
  Koordinator ! {hello, GgtName},
  logging(LogFile, lists:concat(["Beim Koordinator ", to_String(Koordinator), " gemeldet.\n"])),

  Nameservice ! {self(),{bind,GgtName,node()}},
  receive 
    ok -> logging(LogFile, lists:concat([to_String(self()), ",..bind.done.\n"]));
    in_use ->logging(LogFile, lists:concat([to_String(self()), "..schon gebunden.\n"]))
  end,

  receive
  {setneighbors,LeftN,RightN} ->  
      io:fwrite("Linker Nachbar: ~p~n",[LeftN]), io:fwrite("Rechter Nachbar: ~p~n",[RightN])
  end,


loop(Nameservice, Koordinator, GgtName, LeftN, RightN, 1, LogFile, Arbeitszeit, TermZeit, empty).


loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, LogFile, Arbeitszeit, TermZeit, Timer) ->
  receive 
    
    {setpm, MiNeu} ->
      logging(LogFile, lists:concat(["Setpm: ", MiNeu, "\n"])),
      timer:cancel(Timer),
      {ok,NewTimer} = timer:send_after(TermZeit*1000,term),
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, MiNeu, LogFile, Arbeitszeit, TermZeit, NewTimer);

    {sendy,Y} -> 
      
      logging(LogFile, lists:concat(["sendy ", to_String(Y), "; \n"])),
      timer:cancel(Timer),
      {ok,NewTimer} = timer:send_after(TermZeit*1000, self(),{term}),
      timer:sleep(Arbeitszeit*1000),
      {NewMi, CTime} = calculateMi(Y, Mi),
      case NewMi == Mi of
        false -> 
          logging(LogFile, 
            lists:concat(["sendy: ", to_String(Y), " (", to_String(Mi), ") berechnet als neues Mi: ", to_String(NewMi), " ", CTime, "\n"])),
          LeftN ! {sendy,NewMi},
          RightN ! {sendy,NewMi},
          io:fwrite(lists:concat(["informed ", LeftN, "and ", RightN, " with new Mi: ", NewMi, "\n"])),
        
          Koordinator ! {briefmi,{GgtName,NewMi,CTime}};
        
        true ->
          logging(LogFile, lists:concat("sendy: ", to_String(Y), "(", to_String(Mi) ,"); ",  "Keine Berechnung\n"))
      end;

    {kill} -> 
      die(Nameservice, GgtName);
    _ -> 
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, LogFile, Arbeitszeit, TermZeit, Timer)
  end.



die(Nameservice, GgtName) ->
  Nameservice ! {self(),{unbind,GgtName}},
  receive 
    ok -> io:format("..unbind..done.\n")
  end,
  unregister(GgtName).

buildName(Praktikumsgruppe, Teamnummer, GGTProzessZahl) ->
 erlang:list_to_atom(lists:concat([Praktikumsgruppe, Teamnummer, GGTProzessZahl, 1])).
  

calculateMi(Y, Mi) -> 
  
  case (Y < Mi) of
    true -> NewMi = ((Mi-1) rem Y) +1,
            {NewMi, timeMilliSecond()};
    
    false -> {Mi, timeMilliSecond()} 
  end.       


