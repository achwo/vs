-module(run_stuff).
-export([start/0]).

start() ->
  K = koordinator:start(),
  timer:sleep(6000),
  starter:start(K).
