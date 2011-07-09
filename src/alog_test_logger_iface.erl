-module(alog_test_logger_iface).

-behaviour(gen_alogger).

-export([
         start/0,
         stop/0,
         log/2,
         format/6
        ]).

start() -> ok.
stop()  -> ok.

format(FormatString, [RequestRef], Tag, Module, Line, Pid) ->
    Pid ! {format, RequestRef},
    {Pid, RequestRef}.

log(Level, {Pid, RequestRef}) ->
    Pid ! {log, RequestRef, Level}.
    
