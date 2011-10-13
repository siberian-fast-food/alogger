%% @private
%% @doc
%% This module is used while testing with EUnit.
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
