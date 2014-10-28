-module(compileMyShit).
-export([compile/0]).

compile() -> 
	compile:file(nachrichtendienst),
	compile:file(clientlist),
	compile:file(dlq),
	compile:file(hbq),
	compile:file(werkzeug),
	compile:file(client),
	compile:file(server_time),
	application:set_env(server, dlq_max_size, 10).