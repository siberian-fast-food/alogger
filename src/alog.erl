-module(alog).
-behaviour(application).

%% API
-export([start/0, stop/0]).
%% Application callbacks
-export([start/2, stop/1]).

%%% API
start() ->
    ok = alog_error_logger_handler:install(),
    application:start(alog).

stop() ->
    application:stop(alog).

%% Application callbacks
start(_StartType, _StartArgs) ->
    Link = alog_sup:start_link(),
    alog_examples:run_examples(),
    Link.

stop(_State) ->
    ok.
