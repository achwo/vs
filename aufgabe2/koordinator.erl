%{briefmi,{Clientname,CMi,CZeit}}: Ein ggT-Prozess mit Namen Clientname informiert über sein neues Mi CMi um CZeit Uhr. 
%{briefterm,{Clientname,CMi,CZeit},From}: Ein ggT-Prozess mit Namen Clientname und PID From informiert über über die Terminierung der Berechnung mit Ergebnis CMi um CZeit Uhr.
%reset: Der Koordinator sendet allen ggT-Prozessen das kill-Kommando und bringt sich selbst in den initialen Zustand, indem sich Starter wieder melden können.
%step: Der Koordinator beendet die Initialphase und bildet den Ring. Er wartet nun auf den Start einer ggT-Berechnung.
%prompt: Der Koordinator erfragt bei allen ggT-Prozessen per tellmi deren aktuelles Mi ab und zeigt dies im log an.
%nudge: Der Koordinator erfragt bei allen ggT-Prozessen per pingGGT deren Lebenszustand ab und zeigt dies im log an.
%toggle: Der Koordinator verändert den Flag zur Korrektur bei falschen Terminierungsmeldungen.
%{calc,WggT}: Der Koordinator startet eine neue ggT-Berechnung mit Wunsch-ggT WggT.

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
  %io:fwrite("NameserviceName ~p~n", [NameserviceName]),
  Ping = net_adm:ping(NameserviceName),
  timer:sleep(1000),
  %io:fwrite("Ping ~p~n", [Ping]),
  global:whereis_name(nameservice).

start() ->
spawn_link(fun() -> koordinatorStart() end).

koordinatorStart() ->
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
      %io:fwrite("NameserviceNode ~p~n", [Nameservice]),
     
      Nameservice ! {self(),{bind,koordinator,node()}},
      receive ok -> logging(Logfile, "beim Namensdienst registriert.\n");
        in_use -> io:format("Fehler: Name schon gebunden.\n")
      end,
      logging(Logfile, "\n"),
      loop(Nameservice, [], Logfile)
  end.

loop(Nameservice, GgtList, Logfile) ->
  receive 
    {getsteeringval,StarterName} -> 
      % todo: was ist die (0)?
      logging(Logfile, lists:concat(["getsteeringval: ", to_String(StarterName), " (0)."])),
    	StarterName ! {steeringval,config(arbeitszeit),config(termzeit),config(ggtprozessnummer)},
      loop(Nameservice, GgtList, Logfile);

    {hello, GgtName} ->
      % todo: was ist die (3)?
      logging(Logfile, lists:concat(["hello: ", to_String(GgtName), " (3).\n"])),
      % todo: kritisch, wenn name doppelt eingetragen wird?
      GgtListNew = lists:append(GgtList, [GgtName]),
      loop(Nameservice, GgtListNew, Logfile);
   
    {kill} -> die(Nameservice, Logfile);
    _ -> loop(Nameservice, GgtList, Logfile)
  end.

 die(Nameservice, Logfile) ->
  Nameservice ! {self(),{unbind,koordinator}},
  receive 
    ok -> logging(Logfile, "unbound koordinator at nameservice.\n")
  end,
  unregister(koordinator).