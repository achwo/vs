#!/bin/sh

erlc *.erl

erl -name starter@$1 -setcookie karl -noshell -pa ebin -eval "starter:start()" 