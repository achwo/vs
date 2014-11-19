-module(run_stuff).
-export([start/0]).

start() ->
  koordinator:start(),
  timer:sleep(2000),
  starter:start(1).