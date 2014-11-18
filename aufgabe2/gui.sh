#!/bin/bash
erlc werkzeug.erl utility.erl gui.erl
erl -name gui@$1 -setcookie vsp -noshell -pa ebin -eval "gui:start()" -s init stop
