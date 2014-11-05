%{getsteeringval,self()}: Die Anfrage nach den steuernden Werten durch den Starter Prozess.
%{hello,Clientname}: Ein ggT-Prozess meldet sich beim Koordinator mit Namen Clientname an (Name ist der lokal registrierte Name!).
%{briefmi,{Clientname,CMi,CZeit}}: Ein ggT-Prozess mit Namen Clientname informiert über sein neues Mi CMi um CZeit Uhr. 
%{briefterm,{Clientname,CMi,CZeit},From}: Ein ggT-Prozess mit Namen Clientname und PID From informiert über über die Terminierung der Berechnung mit Ergebnis CMi um CZeit Uhr.
%reset: Der Koordinator sendet allen ggT-Prozessen das kill-Kommando und bringt sich selbst in den initialen Zustand, indem sich Starter wieder melden können.
%step: Der Koordinator beendet die Initialphase und bildet den Ring. Er wartet nun auf den Start einer ggT-Berechnung.
%prompt: Der Koordinator erfragt bei allen ggT-Prozessen per tellmi deren aktuelles Mi ab und zeigt dies im log an.
%nudge: Der Koordinator erfragt bei allen ggT-Prozessen per pingGGT deren Lebenszustand ab und zeigt dies im log an.
%toggle: Der Koordinator verändert den Flag zur Korrektur bei falschen Terminierungsmeldungen.
%{calc,WggT}: Der Koordinator startet eine neue ggT-Berechnung mit Wunsch-ggT WggT.
%kill: Der Koordinator wird beendet und sendet allen ggT-Prozessen das kill-Kommando.

-module(koordinator).
-import(werkzeug, [get_config_value/2]).
-export([start/0]).

load_config() ->
  {ok, ConfigFile} = file:consult("koordinator.cfg"),
  
  {ok, Arbeitszeit} = get_config_value(arbeitszeit, ConfigFile),
  application:set_env(koordinator, arbeitszeit, Arbeitszeit),
  
  {ok, Termzeit} = get_config_value(termzeit, ConfigFile),
  application:set_env(koordinator, termzeit, Termzeit),

  {ok, Ggtprozessnummer} = get_config_value(ggtprozessnummer, ConfigFile),
  application:set_env(koordinator, ggtprozessnummer, Ggtprozessnummer).

 config(Key) ->
  {_, Value} = application:get_env(koordinator, Key),
  Value.

findNameService() ->
  NameserviceNode = 'ns@141.22.83.176',
  Ping = net_adm:ping(NameserviceNode),
  NS = global:whereis_name(nameservice),
  io:fwrite("NS: ~p~n",[NS]),
io:fwrite("Ping: ~p~n",[Ping]).

start() ->
  register(koordinator,self()),
  load_config(),
 	
  Nameservice = findNameService(),
 
  {Nameservice, nameservice} ! {self(),{bind,koordinator,node()}},
  receive ok -> io:format("..bind.done.\n");
    in_use -> io:format("..schon gebunden.\n")
  end,
  loop(Nameservice).


loop(Nameservice) ->
  receive 
    {getsteeringval,StarterName} -> 
    	StarterName ! {steeringval,config(arbeitszeit),config(termzeit),config(ggtprozessnummer)};
   

    {kill} -> die(Nameservice);
    _ -> loop(Nameservice)
  end.


 die(Nameservice) ->
  Nameservice ! {self(),{unbind,ggt}},
  receive 
    ok -> io:format("..unbind..done.\n")
  end,
  unregister(ggt).