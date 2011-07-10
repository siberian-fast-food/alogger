%%% @doc This module is a main alog module. It serves start/0 and
%%% stop/0 functions as a user API and implements appication behaviour.
-module(alog).
-behaviour(application).
-include_lib("alog.hrl").

%% API
-export([start/0, stop/0]).
%% Application callbacks
-export([start/2, stop/1]).
%% Runtime logging API
-export([log/4]).
-export([dbg/3, dbg/2, dbg/1]).
-export([info/3, info/2, info/1]).
-export([notice/3, notice/2, notice/1]).
-export([warning/3, warning/2, warning/1]).
-export([error/3, error/2, error/1]).
-export([critical/3, critical/2, critical/1]).
-export([alert/3, alert/2, alert/1]).
-export([emergency/3, emergency/2, emergency/1]).

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
    ok = alog_control:init_loggers(),
    alog_examples:run_examples(),
    Link.

%% @private
stop(_State) ->
    ok.

%%% Runtime logging API
log(Format, Args, Level, Tags) ->
    ?LOGMOD:?LOGFUN(Format, Args, Level, Tags, runtime, 0, self()).

dbg(Format, Args, Tag) -> log(Format, Args, ?debug, Tag).
dbg(Format, Args)      -> log(Format, Args, ?debug, []).
dbg(Format)            -> log(Format, [], ?debug, []).

info(Format, Args, Tag) -> log(Format, Args, ?info, Tag).
info(Format, Args)      -> log(Format, Args, ?info, []).
info(Format)            -> log(Format, [], ?info, []).

notice(Format, Args, Tag) -> log(Format, Args, ?notice, Tag).
notice(Format, Args)      -> log(Format, Args, ?notice, []).
notice(Format)            -> log(Format, [], ?notice, []).

warning(Format, Args, Tag) -> log(Format, Args, ?warning, Tag).
warning(Format, Args)      -> log(Format, Args, ?warning, []).
warning(Format)            -> log(Format, [], ?warning, []).

error(Format, Args, Tag) -> log(Format, Args, ?error, Tag).
error(Format, Args)      -> log(Format, Args, ?error, []).
error(Format)            -> log(Format, [], ?error, []).

critical(Format, Args, Tag) -> log(Format, Args, ?critical, Tag).
critical(Format, Args)      -> log(Format, Args, ?critical, []).
critical(Format)            -> log(Format, [], ?critical, []).

alert(Format, Args, Tag) -> log(Format, Args, ?alert, Tag).
alert(Format, Args)      -> log(Format, Args, ?alert, []).
alert(Format)            -> log(Format, [], ?alert, []).

emergency(Format, Args, Tag) -> log(Format, Args, ?emergency, Tag).
emergency(Format, Args)      -> log(Format, Args, ?emergency, []).
emergency(Format)            -> log(Format, [], ?emergency, []).
