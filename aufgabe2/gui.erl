-module(gui).
-export([start/0]).

start() ->
  net_adm:ping('ns@141.22.83.7'),
  timer:sleep(1000),
  N = global:whereis_name(nameservice),
  N ! {self(), {lookup, koordinator}}, 
  receive 
    {pin, {Name, Node}} -> 
      net_adm:ping(Node),
      timer:sleep(1000),
      Koordinator = global:whereis_name(Name),
      Koordinator ! {step}
      % do stuff
      ; 
    _ -> kacke 
  end.
  