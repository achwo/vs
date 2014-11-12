#!/bin/bash
erlc werkzeug.erl utility.erl gui.erl
erl -name gui@$1 -setcookie karl -noshell -pa ebin -eval "gui:start()"