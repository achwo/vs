#!/bin/sh

erlc *.erl

erl -name koordinator@$1 -setcookie karl -noshell -pa ebin -eval "run_stuff:start()"

# todo starter:start(K). 