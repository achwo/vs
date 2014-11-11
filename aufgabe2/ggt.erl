-module(ggt).
-import(werkzeug,[logging/2, to_String/1, get_config_value/2, timeMilliSecond/0]).
-export([start/8, calculateMi/9]).

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

  GgtName = buildName(Praktikumsgruppe, Teamnummer, GGTProzessZahl, StarterId),
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


loop(Nameservice, Koordinator, GgtName, LeftN, RightN, -99, LogFile, Arbeitszeit, TermZeit, empty, 0, timeMilliSecond()).


loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, LogFile, Arbeitszeit, TermZeit, Timer, TermCount, LastMiTime) ->
  receive 
    
    {setpm, MiNeu} ->
      timer:cancel(Timer),
      {ok,NewTimer} = timer:send_after(TermZeit*1000,{tiTerm}),
      logging(LogFile, lists:concat(["Setpm: ", MiNeu, "\n"])),
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, MiNeu, LogFile, Arbeitszeit, TermZeit, NewTimer, TermCount, timeMilliSecond());

    {sendy,Y} -> 
      timer:cancel(Timer),
      {ok,NewTimer} = timer:send_after(TermZeit*1000,{tiTerm}),
      logging(LogFile, lists:concat(["sendy ", to_String(Y), "; \n"])),
      spawn_link(fun() -> calculateMi(Y, Mi, Koordinator, LeftN, RightN, LogFile, self(), Arbeitszeit, GgtName) end),
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, LogFile, Arbeitszeit, TermZeit, NewTimer, TermCount, timeMilliSecond());

    {tellmi, From} -> 
      From ! {mi,Mi},
      io:fwrite("Aktuelles Mi: ~p~n", [Mi]),
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, LogFile, Arbeitszeit, TermZeit, Timer, TermCount, LastMiTime);

    {pingGGT,From} ->
      From ! {pongGGT,GgtName},
      io:fwrite("PongGGT: ~p~n", [GgtName]),
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, LogFile, Arbeitszeit, TermZeit, Timer, TermCount, LastMiTime);

    {abstimmung, Initiator} -> 
      case Initiator =:= GgtName of
        true -> 
          TermCountNew = TermCount +1,
          CurrentTime = timeMilliSecond(),
          Koordinator ! {briefmi, {GgtName, Mi, CurrentTime}},
          logging(LogFile, lists:concat([GgtName, ": stimme ab (", GgtName, "): Koordinator ", TermCountNew, " Terminierung gemeldet mit ", Mi, ". ", CurrentTime, "\n"]));
          
        false -> 
          Now = timeMilliSecond(),
          DiffTime = Now - LastMiTime,

          case DiffTime >= ((TermZeit*1000)/2) of
            true -> 
              RightN ! {abstimmung, Initiator},  
              logging(LogFile, lists:concat([GgtName, ": stimme ab (", Initiator, "): mit >JA< gestimmt und weitergeleitet ", timeMilliSecond(), "\n"])); 
            false ->
              logging(LogFile, lists:concat([GgtName,": stimme ab (", Initiator, " ): mit >NEIN< gestimmt und ignoriert."]))
          end
      
      end,
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, LogFile, Arbeitszeit, TermZeit, Timer, TermCount, LastMiTime);

    {tiTerm} ->
      logging(LogFile, lists:concat([GgtName, ": initiiere eine Terminierungsabstimmung (", Mi, "). ", timeMilliSecond(), "\n"])),
      RightN ! {abstimmung, GgtName},
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, LogFile, Arbeitszeit, TermZeit, Timer, TermCount, LastMiTime);

    {calcResult, NewMi} -> 
      io:fwrite("Erhalte Berechnung: ~p~n", [NewMi]),
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, NewMi, LogFile, Arbeitszeit, TermZeit, Timer, TermCount, LastMiTime);

    {kill} -> 
      die(Nameservice, GgtName),
      logging(LogFile, lists:concat(["Downtime: ", timeMilliSecond(), " vom Client ", GgtName]));
    
    _ -> 
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, LogFile, Arbeitszeit, TermZeit, Timer, TermCount, LastMiTime)
  end.



die(Nameservice, GgtName) ->
  Nameservice ! {self(),{unbind,GgtName}},
  receive 
    ok -> io:format("..unbind..done.\n")
  end,
  unregister(GgtName).


buildName(Praktikumsgruppe, Teamnummer, GGTProzessZahl, StarterId) ->
 erlang:list_to_atom(lists:concat([Praktikumsgruppe, Teamnummer, GGTProzessZahl, StarterId])).
  

calculateMi(Y, Mi, Koordinator, LeftN, RightN, LogFile, GgtProcess, Arbeitszeit,GgtName) -> 
  case (Y < Mi) of
    true -> NewMi = ((Mi-1) rem Y) +1,
            timer:sleep(Arbeitszeit*1000),
        case NewMi =/= Mi of
          true -> 
            logging(LogFile, lists:concat(["sendy: ", to_String(Y), 
            " (", to_String(Mi), ") berechnet als neues Mi: ", to_String(NewMi), " ", timeMilliSecond(), "\n"])),
            LeftN ! {sendy,NewMi},
            RightN ! {sendy,NewMi},
            io:fwrite(lists:concat(["informed ", LeftN, "and ", RightN, " with new Mi: ", NewMi, "\n"])),
          
            Koordinator ! {briefmi,{GgtName,NewMi,timeMilliSecond()}},
            GgtProcess ! {calcResult, NewMi};
        
          false ->
            logging(LogFile, lists:concat("sendy: ", to_String(Y), "(", to_String(Mi) ,"); ",  "Zahl nach Berechnung gleich geblieben\n")),
            GgtProcess ! {calcResult, Mi}
        end;
    
    false -> 
      logging(LogFile, lists:concat("sendy: ", to_String(Y), "(", to_String(Mi) ,"); ",  "Keine Berechnung\n")),
      GgtProcess ! {calcResult, Mi}
  
  end.       





