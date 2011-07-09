#!/bin/sh
exec erl -pa ebin deps/*/ebin -boot start_sasl \
    -sname alog_dev +K true \
    -s alog \
    -setcookie blah \
    -config ./priv/alog
