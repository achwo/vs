#!/bin/sh

erlc *.erl

erl -name paul@$1 -setcookie kram -noshell -pa ebin -eval "server:start()" 