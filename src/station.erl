-module(station).
-export([start/1]).

start([Interface, MulticastIP, Port, StationType]) ->
  start([Interface, MulticastIP, Port, StationType, '0']);

start([Interface, MulticastIP, Port, StationType, TimeDeviation]) ->
  Interface = ipByInterfaceName(atom_to_list(Interface)),
  {ok,MultiIP} = inet_parse:address(atom_to_list(MulticastIP)),
  {Port,_Unused} = string:to_integer(atom_to_list(Port)),
  StationType = atom_to_list(StationType),

  {TimeDeviation, _Unused} = string:to_integer(atom_to_list(TimeDeviation)),
  
  %Show Info about Station
   outputScreen(MultiIP, Interface, Port, StationType, TimeDeviation),

  %Manager initialisation...  
   SyncManager = sync_manager:start(TimeDeviation, StationType),
   SlotManager = slot_manager:start(SyncManager),

  %Sender initialisation...
   DataSource = data_source:start(),
   Sender = sender:start(SyncManager, SlotManager, Interface, MultiIP, Port, StationType),
   DataSource ! {setListener, Sender},
   SlotManager ! {set_sender, Sender},

  %Receiver initialisation...
   DataSink = data_sink:start(),
   Receiver = receiver:start(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port),

   SlotManager ! {set_receiver, Receiver}.
  

ipByInterfaceName(InterfaceName) ->
  {ok, Interfaces} = inet:getifaddrs(),
  Data = proplists:get_value(InterfaceName, Interfaces),
  Addrs = proplists:lookup_all(addr, Data),
  {ok, Addr} = getIP(Addrs),
  Addr.

getIP([{addr, Addr}|Addrs]) ->
  AddrString = inet:ntoa(Addr),
  getIP(inet:parse_ipv4_address(AddrString), Addrs).

getIP({error, einval}, Addrs) ->
  getIP(Addrs);
getIP(Addr, _Addrs) ->
  Addr.

outputScreen(MultiIP, Interface, Port, StationType, TimeDeviation) ->
  io:format("~n~n"),
  io:format("====================================================~n"),
  io:format("Multicast IP  : ~p~n", [MultiIP]),
  io:format("Interface     : ~p~n", [Interface]),
  io:format("ListenPort    : ~p~n", [Port]),
  io:format("StationType   : ~p~n", [StationType]),
  io:format("Time Deviation: ~p~n", [TimeDeviation]),
  io:format("====================================================~n~n").
  
