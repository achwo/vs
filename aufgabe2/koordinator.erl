-module(koordinator).
-import(werkzeug, [get_config_value/2, logging/2, to_String/1, timeMilliSecond/0]).
-export([start/0]).

load_config() ->
  {ok, ConfigFile} = file:consult("koordinator.cfg"),
  
  {ok, Arbeitszeit} = get_config_value(arbeitszeit, ConfigFile),
  application:set_env(koordinator, arbeitszeit, Arbeitszeit),
  
  {ok, Termzeit} = get_config_value(termzeit, ConfigFile),
  application:set_env(koordinator, termzeit, Termzeit),

  {ok, Ggtprozessnummer} = get_config_value(ggtprozessnummer, ConfigFile),
  application:set_env(koordinator, ggtprozessnummer, Ggtprozessnummer),

  {ok, NameserviceNode} = get_config_value(nameservicenode, ConfigFile),
  application:set_env(koordinator, nameservicenode, NameserviceNode),

  {ok, NameserviceName} = get_config_value(nameservicename, ConfigFile),
  application:set_env(koordinator, nameservicename, NameserviceName).

 config(Key) ->
  {_, Value} = application:get_env(koordinator, Key),
  Value.

findNameService() ->
  NameserviceName = config(nameservicenode),
  net_adm:ping(NameserviceName),
  timer:sleep(1000),
  global:whereis_name(nameservice).

start() ->
  spawn_link(fun() -> run() end).

run() ->
  Logfile = lists:concat(["koordinator_", to_String(node()), ".log"]),
  Startlog = lists:concat([to_String(node()), " Startzeit: ", timeMilliSecond()," mit PID ", to_String(self()), "\n"]),
  logging(Logfile, Startlog),
  load_config(),
  logging(Logfile, "koordinator.cfg gelesen...\n"),
  Nameservice = findNameService(),

  case Nameservice of
    undefined -> logging(Logfile, "Nameservice nicht gefunden...\n");
    _ -> logging(Logfile, "Nameservice gebunden...\n"),
      global:register_name(koordinator,self()),
      logging(Logfile, "lokal registriert...\n"),
     
      Nameservice ! {self(),{bind,koordinator,node()}},
      receive ok -> logging(Logfile, "beim Namensdienst registriert.\n");
        in_use -> io:format("Fehler: Name schon gebunden.\n")
      end,
      logging(Logfile, "\n"),
      initialphase(Nameservice, [], Logfile)
  end.

initialphase(Nameservice, GgtList, Logfile) ->
  receive 
    {getsteeringval,StarterName} -> 
      % todo: was ist die (0)?
      logging(Logfile, lists:concat(["getsteeringval: ", to_String(StarterName), " (0)."])),
    	StarterName ! {steeringval,config(arbeitszeit),config(termzeit),config(ggtprozessnummer)},
      initialphase(Nameservice, GgtList, Logfile);

    {hello, GgtName} ->
      % todo: was ist die (3)?
      logging(Logfile, lists:concat(["hello: ", to_String(GgtName), " (3).\n"])),
      % todo: kritisch, wenn name doppelt eingetragen wird?
      GgtListNew = lists:append(GgtList, [GgtName]),
      initialphase(Nameservice, GgtListNew, Logfile);

    {step} ->
      step(GgtList, Logfile),
      arbeitsphase(Nameservice, [], config(korrigieren), Logfile);

    {reset} ->
    % reset: Der Koordinator sendet allen ggT-Prozessen das kill-Kommando und bringt 
    % sich selbst in den initialen Zustand, indem sich Starter wieder melden können.
      kill_all_ggt(GgtList),
      initialphase(Nameservice, [], Logfile);
   
    {kill} -> beendigungsphase(Nameservice, GgtList, Logfile);
    _ -> initialphase(Nameservice, GgtList, Logfile)
  end.

step(GgtList, Logfile) ->
  logging(Logfile, "step()\n"),
  %todo: missing ggt berechnen
  Missing = missing_ggT(GgtList),
  logging(Logfile, 
    lists:concat(["Anmeldefrist für ggT-Prozesse abgelaufen. Vermisst werden aktuell ", Missing, " ggT-Prozesse."])),
  %todo: bind all ggt
  % ggT-Prozess 488312 (488312) auf ggTs@Brummpa gebunden.
  logging(Logfile, "Alle ggT-Prozesse gebunden.\n"),
  %todo ring erstellen
  %todo ggts ueber nachbarn informieren
  % ggT-Prozess 48832 (ggT@Brummpa) über linken (48813) und rechten (488312) Nachbarn informiert.
  logging(Logfile, "Alle ggT-Prozesse über Nachbarn informiert.\n"),
  logging(Logfile, "Ring wird/wurde erstellt, Koordinator geht in den Zustand 'Bereit für Berechnung'.").

missing_ggT(GgtList) -> config(ggtprozessnummer) - length(GgtList).

arbeitsphase(Nameservice, GgtList, Korrigieren, Logfile) ->
  logging(Logfile, "arbeitsphase()\n"), 
  receive

    {calc, WggT} ->
      %{calc,WggT}: Der Koordinator startet eine neue ggT-Berechnung mit Wunsch-ggT WggT.
      logging(Logfile, lists:concat(["Beginne eine neue ggT-Berechnung mit Ziel ", WggT, ".\n"])),
      %todo: implementation
      %todo: initiale Mis an ggTs senden
      logging(Logfile, lists:concat(["ggT-Prozess 488312 (ggTs@Brummpa) initiales Mi 53444391 gesendet."])),
      logging(Logfile, "Allen ggT-Prozessen ein initiales Mi gesendet."),

      %todo: wievielen ggTs startendes y senden? und anschliessend senden 
      logging(Logfile, lists:concat(["ggT-Prozess 48832 (ggT@Brummpa) startendes y 23154859 gesendet."])),
      logging(Logfile, "Allen ausgewählten ggT-Prozessen ein y gesendet."),
      todo;

    {reset} ->
      kill_all_ggt(GgtList),
      logging(Logfile, "Allen ggT-Prozessen ein 'kill' gesendet.\n"),
      initialphase(Nameservice, [], Logfile);

    {toggle} ->  
      case Korrigieren of
        0 -> NewKorrigieren = 1;
        1 -> NewKorrigieren = 0
      end, 

      logging(Logfile, 
        lists:concat(["toggle des Koordinators um ", timeMilliSecond(), ":", Korrigieren, " zu ", NewKorrigieren, ".\n"])),
      arbeitsphase(Nameservice, GgtList, NewKorrigieren, Logfile);

    {nudge} ->
      % Der Koordinator erfragt bei allen ggT-Prozessen per pingGGT deren Lebenszustand ab und zeigt dies im log an.
      % ggT-Prozess 488312 ist lebendig (01.12 15:51:44,720|).
      % Alle ggT-Prozesse auf Lebendigkeit geprüft.
      todo;

    {kill} -> beendigungsphase(Nameservice, GgtList, Logfile);

    {prompt} ->
      %prompt: Der Koordinator erfragt bei allen ggT-Prozessen per tellmi deren aktuelles Mi ab und zeigt dies im log an.
      todo;
    
    {briefmi, {Clientname, CMi, CZeit}} ->
      %{briefmi,{Clientname,CMi,CZeit}}: Ein ggT-Prozess mit Namen Clientname informiert über sein neues Mi CMi um CZeit Uhr.
      % 488111 meldet neues Mi 280 um "01.12 15:50:28,720|" (01.12 15:50:28,720|).
    todo;

    {briefterm, {Clientname, CMi, CZeit}, From} ->
      %{briefterm,{Clientname,CMi,CZeit},From}: Ein ggT-Prozess mit Namen Clientname und PID From informiert über über die Terminierung der Berechnung mit Ergebnis CMi um CZeit Uhr.
      % 488211 meldet Terminierung mit ggT 525 um "01.12 15:50:26,560|" (01.12 15:50:26,560|).
      % 488112 meldet falsche Terminierung mit ggT 165165 um "01.12 15:50:26,560|" (01.12 15:50:26,560|,525).
      todo;

    _ -> arbeitsphase(Nameservice, GgtList, Korrigieren, Logfile)
  end.

beendigungsphase(Nameservice, GgtList, Logfile) ->
  kill_all_ggt(GgtList),
  logging(Logfile, "Allen ggT-Prozessen ein 'kill' gesendet.\n"),
  Nameservice ! {self(),{unbind,koordinator}},
  receive 
    ok -> logging(Logfile, "Unbound koordinator at nameservice.\n")
  end,
  unregister(koordinator),
  logging(Logfile, 
    lists:concat(["Downtime: ", timeMilliSecond(), " vom Koordinator ", config(koordinatorname), "\n"])).

kill_all_ggt([]) -> ok;
kill_all_ggt([GGT|Rest]) ->
  GGT ! {kill},
  kill_all_ggt(Rest).