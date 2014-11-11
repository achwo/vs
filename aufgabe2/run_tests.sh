#!/bin/bash

erlc koordinator.erl koordinator_tests.erl

erl -noshell -pa ebin -eval "eunit:test(koordinator, [verbose])" -s init stop