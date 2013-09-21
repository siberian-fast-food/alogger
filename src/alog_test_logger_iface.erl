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
%% @private
%% @doc
%% This module is used while testing with EUnit.
%% @end
%% ----------------------------------------------------------------------

-module(alog_test_logger_iface).
-behaviour(gen_alog).

-export([start/2,
         stop/2,
         log/3,
         format/8,
         reload/1]).

%% @private
start(_Name, _) -> ok.

%% @private
stop(_Name, _)  -> ok.

%% @private
format(_FormatString, [RequestRef], Level, Tag, Module, Line, Pid, _Timestamp) ->
    Pid ! {format, RequestRef, Level, Tag, Module, Line, Pid},
    {RequestRef, Level, Tag, Module, Line, Pid}.

reload(_Name) -> ok.

%% @private
log(_Name, Level, {RequestRef, Level, Tag, Module, Line, Pid}) ->
    Pid ! {log, Level, RequestRef, Tag, Module, Line, Pid}.
