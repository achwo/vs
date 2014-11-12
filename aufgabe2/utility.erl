-module(utility).
-export([find_process/2, find_process_with_node/2, find_nameservice/2, log/2, 
  load_config/2, from_config/2]).

find_nameservice(NameserviceNode, NameserviceName) ->
  net_adm:ping(NameserviceNode),
  timer:sleep(1000),
  global:whereis_name(NameserviceName).

find_process(ProcessNameAtom, Nameservice) ->
  {Process, _} = find_process_with_node(ProcessNameAtom, Nameservice),
  Process.

find_process_with_node(ProcessNameAtom, Nameservice) ->
  Nameservice ! {self(), {lookup, ProcessNameAtom}},
  receive 
    {pin, {Name, Node}} -> 
      net_adm:ping(Node),
      timer:sleep(1000),
      {global:whereis_name(Name), Node}; 
    _ -> nok 
  end.

% Logs and attaches \n to message
log(LogFile, Message) ->
  werkzeug:logging(LogFile, Message ++ "\n").

% loads Configfile to application env
load_config(_, []) -> ok;
load_config(ApplicationName, [{Key, Value} | Rest]) ->
  application:set_env(ApplicationName, Key, Value),
  load_config(ApplicationName, Rest).

from_config(ApplicationName, Key) ->
  {_, Value} = application:get_env(ApplicationName, Key),
  Value.