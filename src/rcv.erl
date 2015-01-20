-module(rcv).
-export([start/4]).

start(Parent, Interface, MultiIP, Port) ->
  spawn(fun() -> init(Parent, Interface, MultiIP, Port) end).

init(Parent, Interface, MultiIP, Port) ->
  Socket = werkzeug:openRec(MultiIP, Interface, Port),
  gen_udp:controlling_process(Socket, self()),
  loop(Parent, Socket).

loop(Parent, Socket) ->
  {ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, 0),
  <<StationType:1/binary,
    Payload:24/binary,
    Slot:8/integer,
    Timestamp:64/integer-big>> = Packet,
  Parent ! {message,
    binary_to_list(Payload),
    binary_to_list(StationType),
    Slot,
    Timestamp
  },
  loop(Parent, Socket).
