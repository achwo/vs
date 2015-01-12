-module(station).
-export([start/1]).

start([Interface, MulticastIP, Port, StationType]) ->
  start([Interface, MulticastIP, Port, StationType, '0']);

start([Interface, MulticastIP, Port, StationType, TimeDeviation]) ->
  Interface = get_ip_by_if_name(atom_to_list(Interface)),
  {ok,MultiIP} = inet_parse:address(atom_to_list(MulticastIP)),
  {Port,_Unused} = string:to_integer(atom_to_list(Port)),
  StationType = atom_to_list(StationType),

  {TimeDeviation, _Unused} = string:to_integer(atom_to_list(TimeDeviation)),
  io:format("~n++--------------------------------------------------~n"),
  io:format("++ Starte Station mit Parametern:~n++~n", []),
  io:format("++ Multicast IP    : ~p~n", [MultiIP]),
  io:format("++ Interface       : ~p~n", [Interface]),
  io:format("++ Listen Port     : ~p~n", [Port]),
  io:format("++ Stationsklasse  : ~p~n", [StationType]),
  io:format("++ Zeitverschiebung: ~p~n", [TimeDeviation]),
  io:format("++----------------------------------------------------~n"),


  % todo: our initialization code
  % SyncManager = sync_manager:start(TimeDeviation, StationType),
  % SlotManager = slot_manager:start(SyncManager),

  % % init the sender
  % DataSource = data_source:start(),
  % Sender = sender:start(SyncManager, SlotManager, Interface, MultiIP, Port, StationType),

  % DataSource ! {set_listener, Sender},
  % SlotManager ! {set_sender, Sender},

  % % init the receiver
  % DataSink = data_sink:start(),
  % Receiver = receiver:start(DataSink, SlotManager, SyncManager, Interface, MultiIP, Port),

  % SlotManager ! {set_receiver, Receiver}.
  todo.

get_ip_by_if_name(InterfaceName) ->
  {ok, Interfaces} = inet:getifaddrs(),
  Data = proplists:get_value(InterfaceName, Interfaces),
  Addrs = proplists:lookup_all(addr, Data),
  {ok, Addr} = get_ipv4_address(Addrs),
  Addr.

get_ipv4_address([{addr, Addr}|Addrs]) ->
  AddrString = inet:ntoa(Addr),
  get_ipv4_address(inet:parse_ipv4_address(AddrString), Addrs).

get_ipv4_address({error, einval}, Addrs) ->
  get_ipv4_address(Addrs);
get_ipv4_address(Addr, _Addrs) ->
  Addr.
