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
	 log/3]).

start() ->
    syslog:start(),
    syslog:open("alogger", [cons, perror, pid], daemon).

stop() ->
    ok.

log(Prio, Format, Args) ->
    Msg = lists:flatten(io_lib:format(Format, Args)),
    syslog:log(Prio, Msg).
