-module(log).
-export([start/1, log/4, debug/4]).


log(Logger, Module, Msg, Args) ->
  Logger ! {log, Module, Msg, Args}.

debug(Logger, Module, Msg, Args) ->
  Logger ! {debug, Module, Msg, Args}.


start(Debug) ->
  spawn(fun() -> loop(Debug) end).

loop(Debug) ->
  receive
    {log, Module, Msg, Args} ->
      processLog(Module, Msg, Args);
    {debug, Module, Msg, Args} ->
      processDebug(Module, Msg, Args, Debug)
  end,
  loop(Debug).

processLog(Module, Msg, Args) ->
  NewMsg = "[LOG] " ++ Module ++ ": " ++ Msg,
  write(NewMsg, Args).

processDebug(Module, Msg, Args, true) ->
  NewMsg = "[DEBUG] " ++ Module ++ ": " ++ Msg,
  write(NewMsg, Args);
processDebug(_Module, _Msg, _Args, _Debug) ->
  ok.

write(Msg, Args) ->
  NewMsg = Msg ++ "~n",
  io:format(NewMsg, Args).
