-module(gui).
-export([start/0]).

start() ->
  net_adm:ping('ns@141.22.83.7'),

  N = global:whereis_name(nameservice),
  N ! {self(), {lookup, koordinator}}, 
  receive 
    {pin, {Name, Node}} -> 
      net_adm:ping(Node),
      Koordinator = global:whereis_name(Name),
      % do stuff
      ; 
    _ -> kacke 
  end.
  