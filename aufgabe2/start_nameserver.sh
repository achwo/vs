#!/bin/sh

erlc werkzeug.erl nameservice.erl

erl -name ns@$1 -setcookie karlegon -noshell -pa ebin -eval "nameservice:start()" 