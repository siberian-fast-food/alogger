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

-spec behaviour_info(callbacks) -> [{atom(), integer()}, ...];
                    (_) -> undefined.

behaviour_info(callbacks) ->
    [ {start, 1}
    , {stop, 0}
    , {log, 2}
    , {format, 6}];
behaviour_info(_) ->
    undefined.
