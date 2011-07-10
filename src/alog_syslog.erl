%% @doc
%% The log interface towards system Syslog daemon.
%% @end
%% ----------------------------------------------------------------------
%% Copyright (c) 2011 Siberian Fast Food
%% Authors: Alexander Dergachev <alexander.dergachev@gmail.com>
%%          Artem Golovinsky    <artemgolovinsky@gmail.com>
%%          Igor Karymov        <ingham.k@gmail.com>
%%          Dmitry Groshev      <lambdadmitry@gmail.com>
%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% ----------------------------------------------------------------------

-module(alog_syslog).
-behaviour(gen_alogger).
-include_lib("alog.hrl").

-export([start/1,
         stop/1,
         log/2,
         format/6]).

-define(DEF_IDENT, "alogger").
-define(DEF_LOGOPT, [cons, perror, pid]).
-define(DEF_FACILITY, user).

%% @doc starts syslog driver and opens log with predefined
%%      configuration
-spec start(list()) -> ok.
start(Opts) ->
    Ident = gen_alogger:get_opt(ident, Opts, ?DEF_IDENT),
    Logopt = gen_alogger:get_opt(logopt, Opts, ?DEF_LOGOPT),
    Facility = gen_alogger:get_opt(facility, Opts, ?DEF_FACILITY),
    syslog:start(),
    syslog:open(Ident, Logopt, Facility),
    ok.

%% @doc
-spec stop(list()) -> ok.
stop(_) ->
    ok.

%% @doc logs message Msg with apropriate priority
-spec log(integer(), string()) -> ok.
log(ALoggerPrio, Msg) ->
    SyslogPrio = map_prio(ALoggerPrio),
    syslog:log(SyslogPrio, Msg),
    ok.

%% @doc returns formated log message
-spec format(string(), [term()], string(),
             atom(), integer(), pid()) -> iolist().
format(FormatString, Args, Tag, Module, Line, Pid) ->
    alog_common_formatter:format(FormatString, Args,
                                Tag, Module, Line, Pid).

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
