#!/bin/sh

erl -noshell -pa /var/www/_build/default/lib/*/ebin -config /var/www/config/sys.config -eval 'application:ensure_all_started(mongopool)'
