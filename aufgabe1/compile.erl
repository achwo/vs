-module(compile).

compile() -> 
	c(nachrichtendienst),
	c(clientlist),
	c(dlq),
	c(hbq),
	c(werkzeug),
	application:set_env(server, dlq_max_size, 10).