#!/bin/sh
erlc utility.erl werkzeug.erl koordinator.erl starter.erl ggt.erl run_stuff.erl
erl -name koordinator@$1 -setcookie karl -noshell -pa ebin -eval "run_stuff:start()"
