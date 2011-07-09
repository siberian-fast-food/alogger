%%%----------------------------------------------------------------------
%%% File    : alog_syslog.erl
%%% Author  : Alexander Dergachev <alexander.dergachev@gmail.com>
%%% Purpose :
%%% Created : 09 Jul 2011 by Alexander Dergachev
%%%                          <alexander.dergachev@gmail.com>
%%%
%%%
%%% alogger, Copyright (C) 2011  Siberian Fast Food
%%%----------------------------------------------------------------------

-module(alog_syslog).
-behaviour(gen_alogger).

-export([start/0,
	 stop/0,
	 log/2,
	 format/2]).

-include("alogger.hrl").

-define(IDENT, "alogger").
-define(LOGOPT, [cons, perror, pid]).
-define(FACILITY, user).

%%%----------------------------------------------------------------------
%%% @spec start() -> ok
%%%
%%% @doc starts syslog driver and opens log with predefined
%%%      configuration
%%% @end
%%%----------------------------------------------------------------------
start() ->
    syslog:start(),
    syslog:open(?IDENT, ?LOGOPT, ?FACILITY),
    ok.

%%%----------------------------------------------------------------------
%%% @spec stop() -> ok
%%%
%%% @doc
%%% @end
%%%----------------------------------------------------------------------
stop() ->
    ok.

%%%----------------------------------------------------------------------
%%% @spec log(ALoggerPrio::integer(), Msg::string()) -> ok
%%%
%%% @doc logs message Msg with apropriate priority
%%% @end
%%%----------------------------------------------------------------------
log(ALoggerPrio, Msg) ->
    SyslogPrio = map_prio(ALoggerPrio),
    syslog:log(SyslogPrio, Msg),
    ok.

%%%----------------------------------------------------------------------
%%% @spec format(FormatString::string(), [term()]) -> string()
%%%
%%% @doc returns formated log message
%%% @end
%%%----------------------------------------------------------------------
format(FormatString, Args) ->
    lists:flatten(io_lib:format(FormatString, Args)).

%%%======================================================================
%%% internal functions
%%%======================================================================
%%%----------------------------------------------------------------------
%%% @spec map_prio(ALoggerPrio::integer()) -> atom()
%%%
%%% @doc maps alogger priorities to syslog priorities
%%% @end
%%%----------------------------------------------------------------------
map_prio(?emergency) -> emerg;
map_prio(?alert)     -> alert;
map_prio(?critical)  -> crit;
map_prio(?error)     -> err;
map_prio(?warning)   -> warning;
map_prio(?notice)    -> notice;
map_prio(?info)      -> info;
map_prio(?debug)     -> debug.
