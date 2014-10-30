-module(run_server_tests).
-export([run_all/0]).

run_all() -> 
  compile:file(hbq_tests),
  compile:file(dlq_tests),
  compile:file(werkzeug),
  compile:file(dlq),
  compile:file(hbq),
  eunit:test(hbq),
  eunit:test(dlq).