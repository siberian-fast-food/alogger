-module(alog_test_logger_iface).

-behaviour(gen_alogger).

-export([
         start/1,
         stop/0,
         log/2,
         format/6
        ]).

start(_) -> ok.
stop()  -> ok.

format(_FormatString, [RequestRef], Tag, Module, Line, Pid) ->
    Pid ! {format, RequestRef, Tag, Module, Line, Pid},
    {RequestRef, Tag, Module, Line, Pid}.

log(Level, {RequestRef, Tag, Module, Line, Pid}) ->
    Pid ! {log, Level, RequestRef, Tag, Module, Line, Pid}.

	
    
