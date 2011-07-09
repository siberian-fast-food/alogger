-module(alog).

%% API
-export([start/0, stop/0]).

%%% API
start() ->
    ok = alog_error_logger_handler:install(),
    application:start(alog).

stop() ->
    application:stop(alog).
