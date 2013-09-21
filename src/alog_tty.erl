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
%% Easy logger that logs messages to console (maiking io:format).
%% All functions in this logger are just gen_alogger callbacks, so they
%% are private.
%% @end
%% ----------------------------------------------------------------------

-module(alog_tty).
-behaviour(gen_alog).

%% gen_alogger callbacks
-export([start/2,
         stop/2,
         log/3,
         format/8,
         reload/1]).

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

-spec reload(atom()) -> ok.
reload(_Name) ->
    ok.
