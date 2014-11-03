#!/bin/sh

erlc *.erl

erl -name $1@$2 -setcookie kram -noshell -pa ebin -eval "client:start(\"paul@$3\")" 