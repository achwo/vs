-module(compileTheStuff).
-export([start/0]).

start()->
	compile:file(dispatcher),
	compile:file(server),
	compile:file(server_timer),
	compile:file(clientlist),
	compile:file(werkzeug),
	compile:file(dlq),
	compile:file(hbq).

