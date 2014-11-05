#!/bin/sh

erlc werkzeug.erl nameservice.erl

erl -name starter@$1 -setcookie karl -noshell -pa ebin -eval "starter:start()" 