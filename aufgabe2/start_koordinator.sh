#!/bin/sh

erlc *.erl

erl -name koordinator@$1 -setcookie karl -noshell -pa ebin -eval "K = koordinator:start()"

# todo starter:start(K). 