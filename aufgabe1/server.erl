- module(server).
- export([start/0,loop/0]).

start() -> spawn(server,loop,[]).

loop() -> receive 
           {Client, X} -> Client ! {self(), ok}
          end,
loop.

