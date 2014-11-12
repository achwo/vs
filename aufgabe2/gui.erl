-module(gui).
-export([start/0]).

start() ->
  net_adm:ping('ns@192.168.178.21'),
  timer:sleep(1000),
  N = global:whereis_name(nameservice),
  N ! {self(), {lookup, koordinator}}, 
  receive 
    {pin, {Name, Node}} -> 
      net_adm:ping(Node),
      timer:sleep(1000),
      Koordinator = global:whereis_name(Name),
      Koordinator ! {step}
      % Ggt = global:whereis_name('1121'),
      % % Ggt ! {setneighbors, adolf, joseph}
      % % Ggt ! {setpm, 1982373652472384}
      % Ggt ! {sendy, 129381237}
      % do stuff
      ; 
    _ -> kacke 
  end.
  