-module(alog).

%% API
-export([start/0, stop/0]).

%%% API
start() ->
    application:start(alog).

stop() ->
    application:stop(alog).
