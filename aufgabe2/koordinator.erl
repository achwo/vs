-module(koordinator).
-import(werkzeug, [get_config_value/2, logging/2, to_String/1, timeMilliSecond/0]).
-export([start/0]).
-export([create_ring_tuples/4]).

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
  Startlog = lists:concat([to_String(node()), " Startzeit: ", 
    timeMilliSecond()," mit PID ", to_String(self()), "\n"]),
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
      initialphase(Nameservice, sets:new(), Logfile)
  end.

initialphase(Nameservice, GgTSet, Logfile) ->
  receive 
    {getsteeringval,StarterName} -> 
      % todo: was ist die (0)?
      logging(Logfile, 
        lists:concat(["getsteeringval: ", to_String(StarterName), " (0).\n"])),
    	StarterName ! {steeringval, config(arbeitszeit),
        config(termzeit), config(ggtprozessnummer)},
      initialphase(Nameservice, GgTSet, Logfile);

    {hello, GgtName} ->
      % todo: was ist die (3)?
      logging(Logfile, lists:concat(["hello: ", to_String(GgtName), " (3).\n"])),
      GgTSetNew = sets:add_element(GgtName, GgTSet),
      initialphase(Nameservice, GgTSetNew, Logfile);

    {step} ->
      step(GgTSet, Logfile),
      arbeitsphase(Nameservice, GgTSet, config(korrigieren), Logfile);

    {reset} ->
    % reset: Der Koordinator sendet allen ggT-Prozessen das kill-Kommando und bringt 
    % sich selbst in den initialen Zustand, indem sich Starter wieder melden können.
      kill_all_ggt(GgTSet, Logfile),
      initialphase(Nameservice, sets:new(), Logfile);
   
    {kill} -> beendigungsphase(Nameservice, GgTSet, Logfile);
    _ -> initialphase(Nameservice, GgTSet, Logfile)
  end.

step(GgTSet, Logfile) ->
  logging(Logfile, "step()\n"),
  %todo: missing ggt berechnen
  Missing = missing_ggT(GgTSet),
  logging(Logfile, 
    lists:concat(["Anmeldefrist für ggT-Prozesse abgelaufen. ", 
      "Vermisst werden aktuell ", Missing, " ggT-Prozesse.\n"])),
  %todo: bind all ggt
  % ggT-Prozess 488312 (488312) auf ggTs@Brummpa gebunden.
  logging(Logfile, "Alle ggT-Prozesse gebunden.\n"),
  create_ring(GgTSet, Logfile),
  logging(Logfile, "Ring wird/wurde erstellt, ", 
    "Koordinator geht in den Zustand 'Bereit für Berechnung'.\n").

missing_ggT(GgTSet) -> config(ggtprozessnummer) - length(GgTSet).

arbeitsphase(Nameservice, GgTSet, Korrigieren, Logfile) ->
  logging(Logfile, "arbeitsphase()\n"), 
  receive

    {calc, WggT} ->
      %{calc,WggT}: Der Koordinator startet eine neue ggT-Berechnung mit 
      % Wunsch-ggT WggT.
      %todo: was bedeutet das? Ziel?
      logging(Logfile, lists:concat(["Beginne eine neue ggT-Berechnung mit Ziel ", 
        WggT, ".\n"])),
      %todo: implementation
      %todo: initiale Mis an ggTs senden
      logging(Logfile, lists:concat(["ggT-Prozess 488312 (ggTs@Brummpa) ", 
        "initiales Mi 53444391 gesendet.\n"])),
      logging(Logfile, "Allen ggT-Prozessen ein initiales Mi gesendet.\n"),

      %todo: wievielen ggTs startendes y senden? und anschliessend senden 
      logging(Logfile, lists:concat(["ggT-Prozess 48832 (ggT@Brummpa) ", 
        "startendes y 23154859 gesendet.\n"])),
      logging(Logfile, "Allen ausgewählten ggT-Prozessen ein y gesendet.\n"),
      todo;

    {reset} ->
      kill_all_ggt(GgTSet, Logfile),
      initialphase(Nameservice, [], Logfile);

    {toggle} ->  
      case Korrigieren of
        0 -> NewKorrigieren = 1;
        1 -> NewKorrigieren = 0
      end, 

      logging(Logfile, 
        lists:concat(["toggle des Koordinators um ", timeMilliSecond(), ":", 
          Korrigieren, " zu ", NewKorrigieren, ".\n"])),
      arbeitsphase(Nameservice, GgTSet, NewKorrigieren, Logfile);

    {nudge} ->
      % Der Koordinator erfragt bei allen ggT-Prozessen per pingGGT 
      % deren Lebenszustand ab und zeigt dies im log an.
      % ggT-Prozess 488312 ist lebendig (01.12 15:51:44,720|).
      % Alle ggT-Prozesse auf Lebendigkeit geprüft.
      todo;

    {kill} -> beendigungsphase(Nameservice, GgTSet, Logfile);

    {prompt} ->
      %prompt: Der Koordinator erfragt bei allen ggT-Prozessen per 
      % tellmi deren aktuelles Mi ab und zeigt dies im log an.
      todo;
    
    {briefmi, {Clientname, CMi, CZeit}} ->
      %{briefmi,{Clientname,CMi,CZeit}}: Ein ggT-Prozess mit Namen 
      % Clientname informiert über sein neues Mi CMi um CZeit Uhr.
      logging(Logfile, 
        lists:concat([Clientname, " meldet neues Mi ", CMi, " um ", CZeit, ".\n"])),
      todo;

    {briefterm, {Clientname, CMi, CZeit}, From} ->
      %{briefterm,{Clientname,CMi,CZeit},From}: Ein ggT-Prozess mit Namen Clientname 
      % und PID From informiert über über die Terminierung der Berechnung 
      % mit Ergebnis CMi um CZeit Uhr.
      % todo: falsch berechnen
      Falsch = true,
      case Falsch of
        true -> 
          logging(Logfile, lists:concat([Clientname, 
            " meldet falsche Terminierung mit ggT ", CMi, " um ", CZeit, ".\n"])),
          case Korrigieren of
            true -> % todo: korrigierendes sendy an From,
              From;
            false -> nix
          end;
        false -> 
          logging(Logfile, lists:concat([Clientname, 
            " meldet Terminierung mit ggT ", CMi, " um ", CZeit, ".\n"]))
      end,
      todo;

    _ -> arbeitsphase(Nameservice, GgTSet, Korrigieren, Logfile)
  end.

beendigungsphase(Nameservice, GgTSet, Logfile) ->
  kill_all_ggt(GgTSet, Logfile),
  Nameservice ! {self(),{unbind,koordinator}},
  receive 
    ok -> logging(Logfile, "Unbound koordinator at nameservice.\n")
  end,
  unregister(koordinator),
  logging(Logfile, 
    lists:concat(["Downtime: ", timeMilliSecond(), " vom Koordinator ", 
      config(koordinatorname), "\n"])).

kill_all_ggt(GgTSet, Logfile) ->
  kill_all_ggt(sets:to_list(GgTSet)),
  logging(Logfile, "Allen ggT-Prozessen ein 'kill' gesendet.\n").

kill_all_ggt([]) -> ok;
kill_all_ggt([GgT|Rest]) ->
  GgT ! {kill},
  kill_all_ggt(Rest).

create_ring(GgTSet, Logfile) ->
  GgTList = sets:to_list(GgTSet),
  Pairs = create_ring_tuples(GgTList, none, none, []),
  set_neighbors(Pairs, Logfile).

create_ring_tuples([First|Rest], none, none, Accu) ->
  NewAccu = [{First, lists:last(Rest), lists:nth(1, Rest)}] ++ Accu,
  create_ring_tuples(Rest, First, First, NewAccu);
create_ring_tuples([], _Previous, _First, Accu) -> Accu;
create_ring_tuples([Element], Previous, First, Accu) ->
  create_ring_tuples([], Element, First, [{Element, Previous, First}] ++ Accu);
create_ring_tuples([Element|Rest], Previous, First, Accu) ->
  NewAccu = [{Element, Previous, lists:nth(1, Rest)}] ++ Accu,
  create_ring_tuples(Rest, Element, First, NewAccu).

set_neighbors([], Logfile) -> 
  logging(Logfile, "Alle ggT-Prozesse über Nachbarn informiert.\n");
set_neighbors([{GgT, Left, Right}|Rest], Logfile) ->
  GgT ! {setneighbors, Left, Right},
  logging(Logfile, 
    lists:concat(["ggT-Prozess ", ggT, " über linken (", Left, ")", 
      " und rechten (", Right, ") Nachbarn informiert."])),
  set_neighbors(Rest, Logfile).