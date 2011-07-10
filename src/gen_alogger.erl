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

-export([ behaviour_info/1
	, get_opt/2
	, get_opt/3]).

-spec behaviour_info(callbacks) -> [{atom(), integer()}, ...];
                    (_) -> undefined.

behaviour_info(callbacks) ->
    [ {start, 1}
    , {stop, 0}
    , {log, 2}
    , {format, 6}];
behaviour_info(_) ->
    undefined.

get_opt(Opt, Opts) ->
    case lists:keysearch(Opt, 1, Opts) of
	false ->
	    % TODO: replace with more appropriate function
	    throw({undefined_option, Opt});
	{value, {_, Val}} ->
	    Val
    end.

get_opt(Opt, Opts, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
	false ->
	    Default;
	{value, {_, Val}} ->
	    Val
    end.
