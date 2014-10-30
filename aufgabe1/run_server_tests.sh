#!/bin/sh

erlc werkzeug.erl
erlc hbq.erl hbq_tests.erl
erlc dlq.erl dlq_tests.erl

erl -noshell -pa ebin -eval "eunit:test(hbq, [verbose])" -s init stop
erl -noshell -pa ebin -eval "eunit:test(dlq, [verbose])" -s init stop