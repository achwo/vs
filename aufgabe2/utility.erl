-module(utility).
-export([find_process/2, find_nameservice/2, log/2]).

find_nameservice(NameserviceNode, NameserviceName) ->
  net_adm:ping(NameserviceNode),
  timer:sleep(1000),
  global:whereis_name(NameserviceName).

find_process(ProcessNameAtom, Nameservice) ->
  Nameservice ! {self(), {lookup, ProcessNameAtom}},
  receive 
    {pin, {Name, Node}} -> 
      net_adm:ping(Node),
      timer:sleep(1000),
      global:whereis_name(Name); 
    _ -> nok 
  end.

% Logs and attaches \n to message
log(LogFile, Message) ->
  werkzeug:logging(LogFile, Message ++ "\n").
