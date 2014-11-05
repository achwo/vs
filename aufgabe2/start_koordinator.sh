#!/bin/sh

erlc werkzeug.erl nameservice.erl

erl -name koordinator@$1 -setcookie karl -noshell -pa ebin -eval "koordinator:start()" 