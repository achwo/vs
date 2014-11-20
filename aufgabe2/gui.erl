-module(gui).
-export([start/0]).
-import(werkzeug, [to_String/1]).
-import(utility, [log/2]).

start() ->
  {ok, ConfigFile} = file:consult("koordinator.cfg"),

  Log = lists:concat(["gui.log"]),
  utility:load_config(gui, ConfigFile),
  NNode = utility:from_config(gui, nameservicenode),
  NName = utility:from_config(gui, nameservicename),
  KName = utility:from_config(gui, koordinatorname),
  N = utility:find_nameservice(NNode, NName),

  Koordinator = utility:find_process(KName, N),

  starter:start(2),
  log(Log, "Started 2. Starter"),
  timer:sleep(4000),
  Koordinator ! step,
  log(Log, "Sent step to koordinator."),
  timer:sleep(2000),
  Koordinator ! nudge,
  log(Log, "Sent nudge to koordinator."),
  timer:sleep(2000),
  Koordinator ! toggle,
  log(Log, "Sent toggle to koordinator."),
  timer:sleep(2000),
  % Koordinator ! reset,
  % log(Log, "Sent reset to koordinator."),
  % timer:sleep(2000),
  % Koordinator ! step,
  % log(Log, "Sent step to koordinator."),
  % timer:sleep(2000), 
  Koordinator ! {calc, 3},
  log(Log, "Sent {calc, 3} to koordinator."),
  timer:sleep(12000),
  Koordinator ! prompt,
  log(Log, "Sent prompt to koordinator."),
  timer:sleep(2000),
  Koordinator ! toggle,
  log(Log, "Sent toggle to koordinator."),
  timer:sleep(1000),
  Koordinator ! calc,
  log(Log, "Sent calc to koordinator."),
  timer:sleep(2000),
  Koordinator ! nudge,
  log(Log, "Sent nudge to koordinator."),
  timer:sleep(10000),
  Koordinator ! kill,
  log(Log, "Sent kill to koordinator."),

  ok.