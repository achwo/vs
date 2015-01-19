-module(station).
-export([start/1]).

start([InterfaceAtom, MulticastIPAtom, PortAtom, StationTypeAtom]) ->
  start([InterfaceAtom, MulticastIPAtom, PortAtom, StationTypeAtom, '0']);
start([InterfaceAtom, MulticastIPAtom, PortAtom, StationTypeAtom, TimeDeviationAtom]) ->
  IP = ip(atom_to_list(InterfaceAtom)),
  {ok,MultiIP} = inet_parse:address(atom_to_list(MulticastIPAtom)),
  {Port,_Unused} = string:to_integer(atom_to_list(PortAtom)),
  StationType = atom_to_list(StationTypeAtom),
  {TimeDeviation, _Unused} = string:to_integer(atom_to_list(TimeDeviationAtom)),

  SyncManager = sync_manager:start(TimeDeviation),
  SlotManager = slot_manager:start(SyncManager),
  DataSource = data_source:start(),
  Sender = sender:start(SyncManager, SlotManager, IP, MultiIP, Port, StationType),

  DataSink = data_sink:start(),
  Receiver = receiver:start(DataSink, SlotManager, SyncManager, IP, MultiIP, Port),

  DataSource ! {set_listener, Sender},
  SlotManager ! {set, Sender, Receiver}.

ip(InterfaceName) ->
  {ok, Interfaces} = inet:getifaddrs(),
  Data = proplists:get_value(InterfaceName, Interfaces),
  Addrs = proplists:lookup_all(addr, Data),
  {ok, Addr} = ip4Address(Addrs),
  Addr.

ip4Address([{addr, Addr}|Addrs]) ->
  AddrString = inet:ntoa(Addr),
  ip4Address(inet:parse_ipv4_address(AddrString), Addrs).

ip4Address({error, einval}, Addrs) ->
  ip4Address(Addrs);
ip4Address(Addr, _Addrs) ->
  Addr.
