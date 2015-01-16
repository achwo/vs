-module(log).
-export([start/1, log/4, debug/4, nl/1]).


log(Logger, Module, Msg, Args) ->
  Logger ! {log, Module, Msg, Args}.

debug(Logger, Module, Msg, Args) ->
  Logger ! {debug, Module, Msg, Args}.

nl(Logger) ->
  Logger ! {nl}.


start(Debug) ->
  spawn(fun() -> loop(Debug) end).

loop(Debug) ->
  receive
    {log, Module, Msg, Args} ->
      processLog(Module, Msg, Args);
    {debug, Module, Msg, Args} ->
      processDebug(Module, Msg, Args, Debug);
    {nl} ->
      processNl()
  end,
  loop(Debug).

processLog(Module, Msg, Args) ->
  write("[LOG] ", Module, Msg, Args).

processDebug(Module, Msg, Args, true) ->
  write("[DEBUG] ", Module, Msg, Args);
processDebug(_Module, _Msg, _Args, _Debug) ->
  ok.

processNl() ->
  io:format("~n", []).

write(Prefix, Module, Msg, Args) ->
  ModuleString = io_lib:format("~p: ", [Module]),
  NewMsg = Prefix ++ ModuleString ++ Msg ++ "~n",
  io:format(NewMsg, Args).
