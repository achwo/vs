-module(log).
-export([start/1, log/3, debug/3]).


log(Logger, Msg, Args) ->
  Logger ! {log, Msg, Args}.

debug(Logger, Msg, Args) ->
  Logger ! {debug, Msg, Args}.


start(Debug) ->
  spawn(fun() -> loop(Debug) end).

loop(Debug) ->
  receive
    {log, Msg, Args} ->
      processLog(Msg, Args);
    {debug, Msg, Args} ->
      processDebug(Msg, Args, Debug)
  end,
  loop(Debug).

processLog(Msg, Args) ->
  NewMsg = "[LOG] " ++ Msg,
  write(NewMsg, Args).

processDebug(Msg, Args, true) ->
  NewMsg = "[DEBUG] " ++ Msg,
  write(NewMsg, Args);
processDebug(_Msg, _Args, _Debug) ->
  ok.

write(Msg, Args) ->
  NewMsg = Msg ++ "~n",
  io:format(NewMsg, Args).
