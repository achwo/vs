#!/bin/sh

erlc *.erl

erl -name edgar@$1 -setcookie kram -noshell -pa ebin -eval "client:start(\"paul@$2\")" 