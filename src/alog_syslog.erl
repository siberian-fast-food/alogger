%% ----------------------------------------------------------------------
%% Copyright 2011-2013 alogger project
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc
%% The log interface towards system Syslog daemon.
%% @end
%% ----------------------------------------------------------------------

-module(alog_syslog).
-behaviour(gen_alog).
-include_lib("alog.hrl").

-export([start/2,
         stop/2,
         log/3,
         format/8,
         reload/1]).

-define(DEF_IDENT, "alogger").
-define(DEF_LOGOPT, [cons, perror, pid]).
-define(DEF_FACILITY, user).

%% @doc starts syslog driver and opens log with predefined
%%      configuration
-spec start(atom(), list()) -> ok.
start(_Name, Opts) ->
    Ident = gen_alog:get_opt(ident, Opts, ?DEF_IDENT),
    Logopt = gen_alog:get_opt(logopt, Opts, ?DEF_LOGOPT),
    Facility = gen_alog:get_opt(facility, Opts, ?DEF_FACILITY),
    syslog:start(),
    {ok, Port} = syslog:open(Ident, Logopt, Facility),
    register(?MODULE, Port),
    ok.

%% @doc
-spec stop(atom(), list()) -> ok.
stop(_Name, _) ->
    syslog:stop(?MODULE),
    ok.

%% @doc logs message Msg with apropriate priority
-spec log(atom(), integer(), string()) -> ok.
log(_Name, ALoggerPrio, Msg) ->
    SyslogPrio = map_prio(ALoggerPrio),
    syslog:log(?MODULE, SyslogPrio, Msg),
    ok.

%% @doc returns formated log message
-spec format(string(), [term()], integer(), list(),
             atom(), integer(), pid(),
             {non_neg_integer(), non_neg_integer(), non_neg_integer()}) -> iolist().
format(FormatString, Args, Level, Tag, Module, Line, Pid, TimeStamp) ->
    alog_common_formatter:format(FormatString, Args, Level,
                                Tag, Module, Line, Pid, TimeStamp).

-spec reload(atom()) -> ok.
reload(_Name) ->
    ok.

%%% internal functions
%% @doc maps alogger priorities to syslog priorities
-spec map_prio(integer()) -> atom().
map_prio(?emergency) -> emerg;
map_prio(?alert)     -> alert;
map_prio(?critical)  -> crit;
map_prio(?error)     -> err;
map_prio(?warning)   -> warning;
map_prio(?notice)    -> notice;
map_prio(?info)      -> info;
map_prio(?debug)     -> debug.
