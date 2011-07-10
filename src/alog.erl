%%% @doc This module is a main alog module. It serves start/0 and
%%% stop/0 functions as a user API and implements appication behaviour.
-module(alog).
-behaviour(application).

%% API
-export([start/0, stop/0]).
%% Application callbacks
-export([start/2, stop/1]).

%%% API
%% @doc Starts alog application
start() ->
    ok = alog_error_logger_handler:install(),
    application:start(alog).

%% @doc Stops alog application
stop() ->
    application:stop(alog).

%%% Application callbacks
%% @private
start(_StartType, _StartArgs) ->
    Link = alog_sup:start_link(),
    alog_examples:run_examples(),
    Link.

%% @private
stop(_State) ->
    ok.
