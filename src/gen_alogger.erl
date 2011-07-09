%%%----------------------------------------------------------------------
%%% File    : gen_alogger.erl
%%% Author  : Alexander Dergachev <alexander.dergachev@gmail.com>
%%% Purpose : 
%%% Created : 09 Jul 2011 by Alexander Dergachev 
%%%                          <alexander.dergachev@gmail.com>
%%%
%%%
%%% alogger, Copyright (C) 2011  Siberian Fast Food
%%%----------------------------------------------------------------------

-module(gen_alogger).
-author('alexander.dergachev@gmail.com').

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start, 0},
     {stop, 0},
     {log, 3}];
behaviour_info(_) ->
    undefined.
