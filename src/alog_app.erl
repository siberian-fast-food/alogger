-module(alog_app).

-behaviour(application).

-import_lib("alogger.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% Application callbacks

start(_StartType, _StartArgs) ->
    alog_sup:start_link().

stop(_State) ->
    ok.
