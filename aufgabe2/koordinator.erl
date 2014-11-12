-module(koordinator).
-import(werkzeug, [get_config_value/2, to_String/1, timeMilliSecond/0]).
-import(utility, [log/2, find_process/2]).
-export([start/0]).
-export([create_ring_tuples/4]).

config(Key) -> utility:from_config(koordinator, Key).

start() ->
  spawn_link(fun() -> run() end).

run() ->
  Logfile = lists:concat(["koordinator_", to_String(node()), ".log"]),
  log(Logfile, lists:concat([to_String(node()), " Startzeit: ", 
    timeMilliSecond()," mit PID ", to_String(self())])),
  {ok, ConfigFile} = file:consult("koordinator.cfg"),
  utility:load_config(koordinator, ConfigFile),
  log(Logfile, "koordinator.cfg gelesen..."),
  Nameservice = utility:find_nameservice(
    config(nameservicenode), config(nameservicename)),

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
      kill_all_ggt(GgTSet, Logfile),
      initialphase(Nameservice, sets:new(), Logfile);
   
    {kill} -> beendigungsphase(Nameservice, GgTSet, Logfile);
    _ -> initialphase(Nameservice, GgTSet, Logfile)
  end.

step(GgTSet, Nameservice, Logfile) ->
  log(Logfile, 
    lists:concat(["Anmeldefrist fuer ggT-Prozesse abgelaufen. ", 
      "Vermisst werden aktuell ", missing_ggT(GgTSet), " ggT-Prozesse."])),
  create_ring(GgTSet, Nameservice, Logfile),
  log(Logfile, lists:concat(["Ring wird/wurde erstellt, ", 
    "Koordinator geht in den Zustand 'Bereit fuer Berechnung'."])).

missing_ggT(GgTSet) -> config(ggtprozessnummer) - sets:size(GgTSet).

arbeitsphase(Nameservice, GgTSet, Korrigieren, Log) ->
  log(Log, "arbeitsphase()"), 
  receive

    {calc} ->
      WggT = random:uniform(1000),
      calc(WggT, GgTSet, Nameservice, Log),
      arbeitsphase(Nameservice, GgTSet, Korrigieren, Log);

    {calc, WggT} ->
      calc(WggT, GgTSet, Nameservice, Log),
      arbeitsphase(Nameservice, GgTSet, Korrigieren, Log);

    {reset} ->
      kill_all_ggt(GgTSet, Log),
      initialphase(Nameservice, [], Log);

    {toggle} ->  
      case Korrigieren of
        0 -> NewKorrigieren = 1;
        1 -> NewKorrigieren = 0
      end, 

      log(Log, 
        lists:concat(["toggle des Koordinators um ", timeMilliSecond(), ":", 
          Korrigieren, " zu ", NewKorrigieren, "."])),
      arbeitsphase(Nameservice, GgTSet, NewKorrigieren, Log);

    {nudge} ->
      % Der Koordinator erfragt bei allen ggT-Prozessen per pingGGT 
      % deren Lebenszustand ab und zeigt dies im log an.
      % ggT-Prozess 488312 ist lebendig (01.12 15:51:44,720|).
      % Alle ggT-Prozesse auf Lebendigkeit geprueft.
      todo;

    {kill} -> beendigungsphase(Nameservice, GgTSet, Log);

    {prompt} ->
      %prompt: Der Koordinator erfragt bei allen ggT-Prozessen per 
      % tellmi deren aktuelles Mi ab und zeigt dies im log an.
      todo;
    
    {briefmi, {Clientname, CMi, CZeit}} ->
      %{briefmi,{Clientname,CMi,CZeit}}: Ein ggT-Prozess mit Namen 
      % Clientname informiert ueber sein neues Mi CMi um CZeit Uhr.
      log(Log, 
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
          log(Log, lists:concat([Clientname, 
            " meldet falsche Terminierung mit ggT ", CMi, " um ", CZeit, "."])),
          case Korrigieren of
            true -> % todo: korrigierendes sendy an From,
              From;
            false -> nix
          end;
        false -> 
          log(Log, lists:concat([Clientname, 
            " meldet Terminierung mit ggT ", CMi, " um ", CZeit, "."]))
      end,
      todo;

    _ -> arbeitsphase(Nameservice, GgTSet, Korrigieren, Log)
  end.

calc(WggT, GgTSet, Nameservice, Log) ->
  NumberGgts = sets:size(GgTSet),
  GgTList = sets:to_list(GgTSet),

  log(Log, 
    lists:concat(["Beginne eine neue ggT-Berechnung mit Ziel ", WggT, "."])),
  MiList = werkzeug:bestimme_mis(WggT, NumberGgts),
  send_mis_to_ggts(GgTList, MiList, Nameservice, Log),
  log(Log, "Allen ggT-Prozessen ein initiales Mi gesendet."),

  NumberOfYs = erlang:max(2, (0.15 * NumberGgts)),
  Ys = werkzeug:bestimme_mis(WggT, NumberOfYs),
  Shuffled = werkzeug:shuffle(GgTList),
  send_ys_to_ggts(Shuffled, Ys, Nameservice, Log),
  log(Log, "Allen ausgewaehlten ggT-Prozessen ein y gesendet.").

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

send_mis_to_ggts([], [], _, _) -> ok;
send_mis_to_ggts([GgT|RestGGTs], [Mi|RestMis], Nameservice, Log) -> 
  {Process, Node} = utility:find_process_with_node(GgT, Nameservice),
  Process ! {setpm, Mi},
  log(Log, lists:concat(["ggT-Prozess ", GgT, " (", Node, ") ", 
    "initiales Mi ", Mi, " gesendet."])),
  send_mis_to_ggts(RestGGTs, RestMis, Nameservice, Log).

send_ys_to_ggts(_, [], _, _) -> ok;
send_ys_to_ggts([GgT|RestGGTs], [Y|RestYs], Nameservice, Log) ->
  {Process, Node} = utility:find_process_with_node(GgT, Nameservice),
  log(Log, "Process: " ++ to_String(Process)), 
  % undefined? probably the one that dies of bad arith...
  Process ! {sendy, Y},
  log(Log, lists:concat(["ggT-Prozess ", GgT, " (", Node, ") ", 
    "startendes y ", Y, " gesendet."])),
  send_ys_to_ggts(RestGGTs, RestYs, Nameservice, Log).