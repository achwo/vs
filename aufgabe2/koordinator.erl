-module(koordinator).
-import(werkzeug, [get_config_value/2, to_String/1, timeMilliSecond/0]).
-import(utility, [log/2, find_process/2, find_nameservice/2]).
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
  application:set_env(koordinator, nameservicename, NameserviceName),

  {ok, Korrigieren} = get_config_value(korrigieren, ConfigFile),
  application:set_env(koordinator, korrigieren, Korrigieren).

 config(Key) ->
  {_, Value} = application:get_env(koordinator, Key),
  Value.

start() ->
  spawn_link(fun() -> run() end).

run() ->
  Logfile = lists:concat(["koordinator_", to_String(node()), ".log"]),
  log(Logfile, lists:concat([to_String(node()), " Startzeit: ", 
    timeMilliSecond()," mit PID ", to_String(self())])),
  load_config(),
  log(Logfile, "koordinator.cfg gelesen..."),
  Nameservice = find_nameservice(
    config(nameservicenode), 
    config(nameservicename)),

  case Nameservice of
    undefined -> log(Logfile, "Nameservice nicht gefunden...");
    _ -> log(Logfile, "Nameservice gebunden..."),
      global:register_name(koordinator,self()),
      log(Logfile, "lokal registriert..."),
     
      Nameservice ! {self(),{bind,koordinator,node()}},
      receive ok -> log(Logfile, "beim Namensdienst registriert.");
        in_use -> io:format("Fehler: Name schon gebunden.")
      end,
      log(Logfile, ""), % for \n
      initialphase(Nameservice, sets:new(), Logfile)
  end.

initialphase(Nameservice, GgTSet, Logfile) ->
  receive 
    {getsteeringval,StarterName} -> 
      % todo: was ist die (0)?
      log(Logfile, 
        lists:concat(["getsteeringval: ", to_String(StarterName), " (0)."])),
    	StarterName ! {steeringval, config(arbeitszeit),
        config(termzeit), config(ggtprozessnummer)},
      initialphase(Nameservice, GgTSet, Logfile);

    {hello, GgtName} ->
      % todo: was ist die (3)?
      log(Logfile, lists:concat(["hello: ", to_String(GgtName), " (3)."])),
      GgTSetNew = sets:add_element(GgtName, GgTSet),
      initialphase(Nameservice, GgTSetNew, Logfile);

    {step} ->
      step(GgTSet, Nameservice, Logfile),
      arbeitsphase(Nameservice, GgTSet, config(korrigieren), Logfile);

    {reset} ->
    % reset: Der Koordinator sendet allen ggT-Prozessen das kill-Kommando und bringt 
    % sich selbst in den initialen Zustand, indem sich Starter wieder melden können.
      kill_all_ggt(GgTSet, Logfile),
      initialphase(Nameservice, sets:new(), Logfile);
   
    {kill} -> beendigungsphase(Nameservice, GgTSet, Logfile);
    _ -> initialphase(Nameservice, GgTSet, Logfile)
  end.

step(GgTSet, Nameservice, Logfile) ->
  log(Logfile, 
    lists:concat(["Anmeldefrist fuer ggT-Prozesse abgelaufen. ", 
      "Vermisst werden aktuell ", missing_ggT(GgTSet), " ggT-Prozesse."])),
  %todo: bind all ggt
  % ggT-Prozess 488312 (488312) auf ggTs@Brummpa gebunden.
  log(Logfile, "Alle ggT-Prozesse gebunden."),
  create_ring(GgTSet, Nameservice, Logfile),
  log(Logfile, lists:concat(["Ring wird/wurde erstellt, ", 
    "Koordinator geht in den Zustand 'Bereit fuer Berechnung'."])).

missing_ggT(GgTSet) -> config(ggtprozessnummer) - sets:size(GgTSet).

arbeitsphase(Nameservice, GgTSet, Korrigieren, Logfile) ->
  log(Logfile, "arbeitsphase()"), 
  receive

    {calc, WggT} ->
      %{calc,WggT}: Der Koordinator startet eine neue ggT-Berechnung mit 
      % Wunsch-ggT WggT.
      %todo: was bedeutet das? Ziel?
      log(Logfile, lists:concat(["Beginne eine neue ggT-Berechnung mit Ziel ", 
        WggT, "."])),
      %todo: implementation
      %todo: initiale Mis an ggTs senden
      log(Logfile, lists:concat(["ggT-Prozess 488312 (ggTs@Brummpa) ", 
        "initiales Mi 53444391 gesendet."])),
      log(Logfile, "Allen ggT-Prozessen ein initiales Mi gesendet."),

      %todo: wievielen ggTs startendes y senden? und anschliessend senden 
      log(Logfile, lists:concat(["ggT-Prozess 48832 (ggT@Brummpa) ", 
        "startendes y 23154859 gesendet."])),
      log(Logfile, "Allen ausgewählten ggT-Prozessen ein y gesendet."),
      todo;

    {reset} ->
      kill_all_ggt(GgTSet, Logfile),
      initialphase(Nameservice, [], Logfile);

    {toggle} ->  
      case Korrigieren of
        0 -> NewKorrigieren = 1;
        1 -> NewKorrigieren = 0
      end, 

      log(Logfile, 
        lists:concat(["toggle des Koordinators um ", timeMilliSecond(), ":", 
          Korrigieren, " zu ", NewKorrigieren, "."])),
      arbeitsphase(Nameservice, GgTSet, NewKorrigieren, Logfile);

    {nudge} ->
      % Der Koordinator erfragt bei allen ggT-Prozessen per pingGGT 
      % deren Lebenszustand ab und zeigt dies im log an.
      % ggT-Prozess 488312 ist lebendig (01.12 15:51:44,720|).
      % Alle ggT-Prozesse auf Lebendigkeit geprueft.
      todo;

    {kill} -> beendigungsphase(Nameservice, GgTSet, Logfile);

    {prompt} ->
      %prompt: Der Koordinator erfragt bei allen ggT-Prozessen per 
      % tellmi deren aktuelles Mi ab und zeigt dies im log an.
      todo;
    
    {briefmi, {Clientname, CMi, CZeit}} ->
      %{briefmi,{Clientname,CMi,CZeit}}: Ein ggT-Prozess mit Namen 
      % Clientname informiert ueber sein neues Mi CMi um CZeit Uhr.
      log(Logfile, 
        lists:concat([Clientname, " meldet neues Mi ", CMi, " um ", CZeit, "."])),
      todo;

    {briefterm, {Clientname, CMi, CZeit}, From} ->
      %{briefterm,{Clientname,CMi,CZeit},From}: Ein ggT-Prozess mit Namen Clientname 
      % und PID From informiert ueber ueber die Terminierung der Berechnung 
      % mit Ergebnis CMi um CZeit Uhr.
      % todo: falsch berechnen
      Falsch = true,
      case Falsch of
        true -> 
          log(Logfile, lists:concat([Clientname, 
            " meldet falsche Terminierung mit ggT ", CMi, " um ", CZeit, "."])),
          case Korrigieren of
            true -> % todo: korrigierendes sendy an From,
              From;
            false -> nix
          end;
        false -> 
          log(Logfile, lists:concat([Clientname, 
            " meldet Terminierung mit ggT ", CMi, " um ", CZeit, "."]))
      end,
      todo;

    _ -> arbeitsphase(Nameservice, GgTSet, Korrigieren, Logfile)
  end.

beendigungsphase(Nameservice, GgTSet, Logfile) ->
  kill_all_ggt(GgTSet, Logfile),
  Nameservice ! {self(),{unbind,koordinator}},
  receive 
    ok -> log(Logfile, "Unbound koordinator at nameservice.")
  end,
  unregister(koordinator),
  log(Logfile, 
    lists:concat(["Downtime: ", timeMilliSecond(), " vom Koordinator ", 
      config(koordinatorname)])).

kill_all_ggt(GgTSet, Logfile) ->
  kill_all_ggt(sets:to_list(GgTSet)),
  log(Logfile, "Allen ggT-Prozessen ein 'kill' gesendet.").

kill_all_ggt([]) -> ok;
kill_all_ggt([GgT|Rest]) ->
  GgT ! {kill},
  kill_all_ggt(Rest).

create_ring(GgTSet, Nameservice, Logfile) ->
  GgTList = sets:to_list(GgTSet),
  Pairs = create_ring_tuples(GgTList, none, none, []),
  set_neighbors(Pairs, Nameservice, Logfile).

create_ring_tuples([First|Rest], none, none, Accu) ->
  NewAccu = [{First, lists:last(Rest), lists:nth(1, Rest)}] ++ Accu,
  create_ring_tuples(Rest, First, First, NewAccu);
create_ring_tuples([], _Previous, _First, Accu) -> Accu;
create_ring_tuples([Element], Previous, First, Accu) ->
  create_ring_tuples([], Element, First, [{Element, Previous, First}] ++ Accu);
create_ring_tuples([Element|Rest], Previous, First, Accu) ->
  NewAccu = [{Element, Previous, lists:nth(1, Rest)}] ++ Accu,
  create_ring_tuples(Rest, Element, First, NewAccu).

set_neighbors([], _, Logfile) -> 
  log(Logfile, "Alle ggT-Prozesse ueber Nachbarn informiert.");
set_neighbors([{GgTName, Left, Right}|Rest], Nameservice, Logfile) ->
  GgTProcess = find_process(GgTName, Nameservice),
  GgTProcess ! {setneighbors, Left, Right},

  log(Logfile, 
    lists:concat(["ggT-Prozess ", GgTName, "(", to_String(GgTProcess), 
      ") ueber linken (", Left, ")", 
      " und rechten (", Right, ") Nachbarn informiert."])),
  set_neighbors(Rest, Nameservice, Logfile).