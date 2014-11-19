-module(gui).
-export([start/0]).
-import(werkzeug, [to_String/1]).
-import(utility, [log/2]).

start() ->
  {ok, ConfigFile} = file:consult("koordinator.cfg"),
  utility:load_config(gui, ConfigFile),
  NNode = utility:from_config(gui, nameservicenode),
  NName = utility:from_config(gui, nameservicename),
  N = utility:find_nameservice(NNode, NName),
  Koordinator = utility:find_process(chef, N),
  
  Log = lists:concat(["gui.log"]),

  Koordinator ! step,
  log(Log, "Sent step to koordinator."),
  timer:sleep(4000),
  Koordinator ! nudge,
  log(Log, "Sent nudge to koordinator."),
  timer:sleep(1000),
  Koordinator ! toggle,
  log(Log, "Sent toggle to koordinator."),
  timer:sleep(1000),
  Koordinator ! reset,
  log(Log, "Sent reset to koordinator."),
  timer:sleep(1000),
  starter:start(1),
  log(Log, "Startet 1 Starter"),
  timer:sleep(4000),
  Koordinator ! {calc, 3},
  log(Log, "Sent {calc, 3} to koordinator."),
  timer:sleep(1000),
  Koordinator ! prompt,
  log(Log, "Sent prompt to koordinator."),
  timer:sleep(1000),
  Koordinator ! toggle,
  log(Log, "Sent toggle to koordinator."),
  timer:sleep(10000),
  Koordinator ! calc,
  log(Log, "Sent calc to koordinator."),
  timer:sleep(1000),
  Koordinator ! nudge,
  log(Log, "Sent nudge to koordinator."),
  timer:sleep(10000),
  Koordinator ! kill,
  log(Log, "Sent kill to koordinator.").