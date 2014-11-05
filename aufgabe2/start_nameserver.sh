#!/bin/sh

erlc werkzeug.erl nameservice.erl

erl -name ns@$1 -setcookie karl -noshell -pa ebin -eval "nameservice:start()" 