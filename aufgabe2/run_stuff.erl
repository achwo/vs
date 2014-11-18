-module(run_stuff).
-export([start/0]).

start() ->
  {ok, ConfigFile} = file:consult("koordinator.cfg"),
  {ok, NameserviceNode} = werkzeug:get_config_value(nameservicenode, ConfigFile),
  {ok, NameserviceName} = werkzeug:get_config_value(nameservicename, ConfigFile),

  NS = utility:find_nameservice(NameserviceNode, NameserviceName),

  NS ! {self(), reset},
  receive _ -> nix end,

  koordinator:start(),
  timer:sleep(2000),
  starter:start(1).
