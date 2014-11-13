-module(ggt).
-import(werkzeug,[to_String/1, get_config_value/2, timeMilliSecond/0]).
-import(utility, [log/2, current_time_millis/0]).
-export([start/8, calculateMi/9]).

%{setneighbors,LeftN,RightN}: die (lokal auf deren Node registrieten und im 
  % Namensdienst registriexrten) Namen des linken und rechten Nachbarn 
% werden gesetzt.
%{setpm,MiNeu}: die von diesem Prozess zu berabeitenden Zahl für eine neue 
% Berechnung wird gesetzt.
%{sendy,Y}: der rekursive Aufruf der ggT Berechnung.
%{abstimmung,Initiator}: Wahlnachricht für die Terminierung der aktuellen 
% Berechnung; Initiator ist der Initiator dieser Wahl 
% (z.B. Name des ggT-Prozesses).
%{tellmi,From}: Sendet das aktuelle Mi an From: From ! {mi,Mi}. 
% Wird vom Koordinator z.B. genutzt, um bei einem Berechnungsstillstand 
% die Mi-Situation im Ring anzuzeigen.
%{pingGGT,From}: Sendet ein pongGGT an From: From ! {pongGGT,GGTname}. 
% Wird vom Koordinator z.B. genutzt, um auf manuelle Anforderung hin die 
% Lebendigkeit des Rings zu prüfen.
%kill: der ggT-Prozess wird beendet.

start(StarterId, GGTProzessZahl, Arbeitszeit, TermZeit, Nameservice, 
  Koordinator, Praktikumsgruppe, Teamnummer) ->
  
  LogFile = lists:concat(["GGTP_", to_String(node()), ".log"]),
  log(LogFile, lists:concat(
    [Praktikumsgruppe, Teamnummer, GGTProzessZahl, StarterId, 
      " Startzeit:", timeMilliSecond(), "mit ", to_String(node()), 
      "auf ", to_String(self()), " "])
  ),

  GgtName = buildName(Praktikumsgruppe, Teamnummer, GGTProzessZahl, StarterId),
  log(LogFile, lists:concat(["Build Ggt-Name: ", to_String(GgtName)])),
  global:register_name(GgtName,self()),
  
  Koordinator ! {hello, GgtName},
  log(LogFile, lists:concat(["Beim Koordinator ", to_String(Koordinator), 
    " gemeldet."])),

  Nameservice ! {self(),{bind,GgtName,node()}},
  receive 
    ok -> log(LogFile, lists:concat([GgtName, " gebunden."]));
    in_use -> 
      log(LogFile, lists:concat(["Fehler: ", GgtName, " schon gebunden."]))
  end,

  receive
  {setneighbors,LeftN,RightN} ->  
    log(LogFile, lists:concat(["Linker Nachbar: ", LeftN])), 
    log(LogFile, lists:concat(["Rechter Nachbar: ", RightN])),
    LeftNProcess = utility:find_process(LeftN, Nameservice),
    RightNProcess = utility:find_process(RightN, Nameservice)
  end,

  loop(Nameservice, Koordinator, GgtName, LeftNProcess, RightNProcess, -99, 
    LogFile, Arbeitszeit, TermZeit, empty, 0, current_time_millis()).


loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, LogFile, 
  Arbeitszeit, TermZeit, Timer, TermCount, LastMiTime) ->
  receive 
    
    {setpm, MiNeu} ->
      case Mi =/= -99 of
        true -> 
          timer:cancel(Timer),
          log(LogFile, lists:concat(["Setpm: Timer cancel and start new Timer"])),
          {ok,NewTimer} = timer:send_after(TermZeit*1000, self(), {tiTerm}),
          log(LogFile, lists:concat(["Setpm: start new Timer after cancel: ", to_String(NewTimer)]));       
        false ->
          {ok,NewTimer} = timer:send_after(TermZeit*1000, self(), {tiTerm}),
          log(LogFile, lists:concat(["Setpm: start new Timer: ", to_String(NewTimer)]))
      end,
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, MiNeu, 
        LogFile, Arbeitszeit, TermZeit, NewTimer, TermCount, current_time_millis());

    {sendy,Y} -> 
      timer:cancel(Timer),
      log(LogFile, lists:concat(["Sendy: Timer cancel and start new Timer"])),
      {ok,NewTimer} = timer:send_after(TermZeit*1000, self(), {tiTerm}),
      log(LogFile, lists:concat(["Sendy: start new Timer after cancel: ", to_String(NewTimer)])),        
      log(LogFile, lists:concat(["sendy ", to_String(Y), "; "])),
      ProcessName = self(),
      spawn_link(fun() -> 
        calculateMi(Y, Mi, Koordinator, LeftN, RightN, LogFile, 
          ProcessName, Arbeitszeit, GgtName) 
      end),
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, 
        LogFile, Arbeitszeit, TermZeit, NewTimer, TermCount, current_time_millis());

    {tellmi, From} -> 
      From ! {mi,Mi},
      log(LogFile, lists:concat(["Aktuelles Mi: ", Mi])),
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, LogFile, 
        Arbeitszeit, TermZeit, Timer, TermCount, LastMiTime);

    {pingGGT,From} ->
      From ! {pongGGT,GgtName},
      log(LogFile, lists:concat(["PongGGT: ", GgtName])),
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, LogFile, 
        Arbeitszeit, TermZeit, Timer, TermCount, LastMiTime);

    {abstimmung, Initiator} -> 
      case Initiator =:= GgtName of
        true -> 
          TermCountNew = TermCount +1,
          CurrentTime = timeMilliSecond(),
          Koordinator ! {briefmi, {GgtName, Mi, CurrentTime}},
          log(LogFile, lists:concat([GgtName, ": stimme ab (", GgtName, "): ", 
            "Koordinator ", TermCountNew, " Terminierung gemeldet mit ", Mi, 
            ". ", CurrentTime]));
          
        false -> 
          Now = (current_time_millis() / 1000),
          log(LogFile, lists:concat(["NowTime: ", to_String(Now)])),
          
          
          NewLastTime = (LastMiTime / 1000), 
          log(LogFile, lists:concat(["NewLastTime: ", to_String(NewLastTime)])),
          
          DiffTime = Now - NewLastTime,
          log(LogFile, lists:concat(["DiffTime: ", to_String(DiffTime)])),

          case DiffTime >= ((TermZeit*1000)/2) of
            true -> 
              RightN ! {abstimmung, Initiator},  
              log(LogFile, lists:concat([GgtName, ": stimme ab (", 
                Initiator, "): mit >JA< gestimmt und weitergeleitet ", 
              timeMilliSecond()])); 
            false ->
              log(LogFile, lists:concat([GgtName,": stimme ab (", Initiator, 
                "): mit >NEIN< gestimmt und ignoriert."]))
          end
      
      end,
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, LogFile, 
        Arbeitszeit, TermZeit, Timer, TermCount, LastMiTime);

    {tiTerm} ->
      log(LogFile, lists:concat([GgtName, ": initiiere eine ", 
        "Terminierungsabstimmung (", Mi, "). ", timeMilliSecond()])),
      RightN ! {abstimmung, GgtName},
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, LogFile, 
        Arbeitszeit, TermZeit, Timer, TermCount, LastMiTime);

    {calcResult, NewMi} -> 
      log(LogFile, lists:concat(["Erhalte Berechnung: ", NewMi])),
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, NewMi, LogFile, 
        Arbeitszeit, TermZeit, Timer, TermCount, LastMiTime);

    {kill} -> die(Nameservice, GgtName, LogFile);
    
    _ -> 
      loop(Nameservice, Koordinator, GgtName, LeftN, RightN, Mi, LogFile, 
        Arbeitszeit, TermZeit, Timer, TermCount, LastMiTime)
  end.

die(Nameservice, GgtName, LogFile) ->
  Nameservice ! {self(),{unbind,GgtName}},
  receive 
    ok -> log(LogFile, lists:concat([GgtName, " unbound."]))
  end,
  unregister(GgtName),
  log(LogFile, 
    lists:concat(["Downtime: ", timeMilliSecond(), " vom Client ", GgtName])).

buildName(Praktikumsgruppe, Teamnummer, GGTProzessZahl, StarterId) ->
  erlang:list_to_atom(
    lists:concat([Praktikumsgruppe, Teamnummer, GGTProzessZahl, StarterId])).
  
calculateMi(Y, Mi, Koordinator, LeftN, RightN, LogFile, GgtProcess, 
  Arbeitszeit, GgtName) -> 
  case (Y < Mi) of
    true -> 
      NewMi = ((Mi-1) rem Y) +1, 
      timer:sleep(Arbeitszeit*1000),
      case NewMi =/= Mi of
        true -> 
          log(LogFile, lists:concat(["sendy: ", to_String(Y), " (", 
            to_String(Mi), ") berechnet als neues Mi: ", to_String(NewMi), 
            " ", timeMilliSecond()])),
          LeftN ! {sendy,NewMi},
          RightN ! {sendy,NewMi},
          log(LogFile, 
            lists:concat(["informed ", to_String(LeftN), "and ", 
              to_String(RightN), " with new Mi: ", NewMi])),
        
          Koordinator ! {briefmi,{GgtName,NewMi,timeMilliSecond()}},
          log(LogFile, lists:concat(["An Koordinator gesenden: ", to_String(Koordinator)])),
          GgtProcess ! {calcResult, NewMi},
          log(LogFile, lists:concat(["An GgtProcess gesenden: ", to_String(GgtProcess)]));
      
        false ->
          log(LogFile, 
            lists:concat(["sendy: ", to_String(Y), "(", to_String(Mi) ,"); ",  
            "Zahl nach Berechnung gleich geblieben"]))
      end;
    
    false -> 
      log(LogFile, lists:concat(["sendy: ", to_String(Y), "(", to_String(Mi) ,"); Keine Berechnung"]))
  end.

