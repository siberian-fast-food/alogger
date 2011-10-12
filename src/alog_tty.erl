%% @doc
%% Easy logger that logs messages to console (maiking io:format).
%% All functions in this logger are just gen_alogger callbacks, so they
%% are private.
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

-module(alog_tty).
-behaviour(gen_alog).

%% gen_alogger callbacks
-export([start/2,
         stop/2,
         log/3,
         format/8]).

%% @private
-spec start(atom(), list()) -> ok.
start(_Name, _) ->
    ok.

%% @private
-spec stop(atom(), list()) -> ok.
stop(_Name, _) ->
    ok.

%% @private
-spec log(atom(), integer(), string()) -> ok.
log(_Name, _ALoggerPrio, Msg) ->
    case node(group_leader()) =:= node() of
        true ->
            io:format("~s", [Msg]);
        _ ->
            ok
    end.

%% @private
-spec format(string(), [term()], integer(), list(),
             atom(), integer(), pid(),
             {non_neg_integer(), non_neg_integer(), non_neg_integer()})
            -> iolist().
format(FormatString, Args, Level, Tag, Module, Line, Pid, TimeStamp) ->
    alog_common_formatter:format(FormatString, Args, Level,
                                Tag, Module, Line, Pid, TimeStamp).
