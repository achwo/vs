-module(utility).
-export([find_process/2, find_process_with_node/2, find_nameservice/2, log/2, 
  load_config/2, from_config/2, current_time_millis/0]).

find_nameservice(NameserviceNode, NameserviceName) ->
  meet(NameserviceNode),
  global:whereis_name(NameserviceName).

meet(Node) ->
  case is_in_nodelist(Node) of
    false -> 
      net_adm:ping(Node),
      timer:sleep(500);
    _ -> ok
  end.

is_in_nodelist(Node) ->
  Nodelist = nodes(),
  find_node(Nodelist, Node).

find_node([], _) -> false;
find_node([Element|Rest], Node) when Element == Node -> true;
find_node([_|Rest], Node) -> find_node(Rest, Node).

find_process(ProcessNameAtom, Nameservice) ->
  {Process, _} = find_process_with_node(ProcessNameAtom, Nameservice),
  Process.

find_process_with_node(ProcessNameAtom, Nameservice) ->
  Nameservice ! {self(), {lookup, ProcessNameAtom}},
  receive 
    {pin, {Name, Node}} -> 
      meet(Node),
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

%-spec get_timestamp() -> integer().
current_time_millis() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).


  
