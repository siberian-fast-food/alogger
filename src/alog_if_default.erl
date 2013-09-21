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
%% This module is a blank for constructing parse_transformed module that
%% makes actual logging.
%% @end
%% ----------------------------------------------------------------------

-module(alog_if_default).

-compile([{parse_transform, alog_parse_trans}, nowarn_unused_vars]).
-include("alog.hrl").
-export([log/7,
         default_mod_ast/0,
         default_modlogs_ast/0]).

%% @doc Will return list of loggers for module/tag
get_mod_logs(_, _, _, _) ->
    [].

%% @doc Main logging function
log(Format, Args, Level, Tags, Module, Line, Pid)
    when is_list(Tags), Tags /= [] ->
    [log(Format, Args, Level, Tag, Module, Line, Pid)
     || Tag <- Tags],
    ok;
log(Format, Args, Level, Tag, Module, Line, Pid) ->
    to_log(Format, Args, Level, Tag,  Module, Line, Pid, now(), get_loggers(Level, Module, Tag)).

%% @doc Get all loggers as list of lists
get_loggers(Level, Module, Tag) ->
    [get_mod_logs(Flow, Level, Module, Tag) || Flow <- flows()].

%% @doc Helpful log function
to_log(Format, Args, Level, Tag, Module, Line, Pid, Time, [FullLoggers | TailLoggers]) ->
    [begin
         Formatted = Formatter:format(Format, Args, Level, Tag, Module, Line, Pid, Time),
         LogMod:log(LogName, Level, Formatted)
     end || {{LogName, LogMod}, Formatter} <- FullLoggers],
    to_log(Format, Args, Level, Tag,  Module, Line, Pid, Time, TailLoggers);
to_log(_, _, _, _, _, _, _, _, []) ->
    ok.

%% @doc Return list of flows, which should be logged
flows() ->
    [].

%% @doc Will return default AST of this module after parse_transform
default_mod_ast() ->
    ok.

%% @doc Will return default AST of get_logs_mod/3
default_modlogs_ast() ->
    ok.
