-module(log).
-export([start/2, log/4, debug/4, nl/1]).


log(Logger, Module, Msg, Args) ->
  Logger ! {log, Module, Msg, Args}.

debug(Logger, Module, Msg, Args) ->
  Logger ! {debug, Module, Msg, Args}.

nl(Logger) ->
  Logger ! {nl}.


start(File, Debug) ->
  spawn(fun() -> loop(File, Debug) end).

loop(File, Debug) ->
  receive
    {log, Module, Msg, Args} ->
      processLog(File, Module, Msg, Args);
    {debug, Module, Msg, Args} ->
      processDebug(File, Module, Msg, Args, Debug);
    {nl} ->
      processNl(File, Debug)
  end,
  loop(File, Debug).

processLog(File, Module, Msg, Args) ->
  write(File, "[LOG] ", Module, Msg, Args).

processDebug(File, Module, Msg, Args, true) ->
  write(File, "[DEBUG] ", Module, Msg, Args);
processDebug(_File, _Module, _Msg, _Args, _Debug) ->
  ok.

processNl(File, true) ->
  % file:write_file(File, io_lib:fwrite("~n", [])),
  io:format("~n", []);
processNl(_, _) ->
  ok.

write(File, Prefix, Module, Msg, Args) ->
  ModuleString = io_lib:format("~p: ", [Module]),
  NewMsg = Prefix ++ ModuleString ++ Msg ++ "~n",
  % file:write_file(File, io_lib:fwrite(NewMsg, [Args]), [append]),
  io:format(NewMsg, Args).
