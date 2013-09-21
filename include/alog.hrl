%% @doc
%% This file contains logging API of alogger and some additional
%% macroses such as log levels.
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

-define(emergency, 0). % system is unusable
-define(alert,     1). % action must be taken immediately
-define(critical,  2). % critical conditions
-define(error,     3). % error conditions
-define(warning,   4). % warning conditions
-define(notice,    5). % normal but significant condition
-define(info,      6). % informational
-define(debug,     7). % debug-level messages

-define(LOGMOD, alog_if).
-define(LOGFUN, log).
-define(LOG(Format, Args, Level, Tags),
        ?LOGMOD:?LOGFUN(Format, Args, Level, Tags, ?MODULE, ?LINE, self())).
-define(LOG(Format, Args, Level, Tags, IDFun),
        ?LOGMOD:?LOGFUN(Format, Args, Level, Tags, ?MODULE, ?LINE, IDFun())).

-define(DBG(Format, Args, Tag), ?LOG(Format, Args, ?debug, Tag)).
-define(DBG(Format, Args),      ?DBG(Format, Args, [])).
-define(DBG(Format),            ?DBG(Format, [])).

-define(DEBUG(Format, Args, Tag), ?LOG(Format, Args, ?debug, Tag)).
-define(DEBUG(Format, Args),      ?DEBUG(Format, Args, [])).
-define(DEBUG(Format),            ?DEBUG(Format, [])).

-define(INFO(Format, Args, Tag), ?LOG(Format, Args, ?info, Tag)).
-define(INFO(Format, Args),      ?INFO(Format, Args, [])).
-define(INFO(Format),            ?INFO(Format, [])).

-define(NOTICE(Format, Args, Tag), ?LOG(Format, Args, ?notice, Tag)).
-define(NOTICE(Format, Args),      ?NOTICE(Format, Args, [])).
-define(NOTICE(Format),            ?NOTICE(Format, [])).

-define(WARNING(Format, Args, Tag), ?LOG(Format, Args, ?warning, Tag)).
-define(WARNING(Format, Args),      ?WARNING(Format, Args, [])).
-define(WARNING(Format),            ?WARNING(Format, [])).

-define(ERROR(Format, Args, Tag), ?LOG(Format, Args, ?error, Tag)).
-define(ERROR(Format, Args),      ?ERROR(Format, Args, [])).
-define(ERROR(Format),            ?ERROR(Format, [])).

-define(CRITICAL(Format, Args, Tag), ?LOG(Format, Args, ?critical, Tag)).
-define(CRITICAL(Format, Args),      ?CRITICAL(Format, Args, [])).
-define(CRITICAL(Format),            ?CRITICAL(Format, [])).

-define(ALERT(Format, Args, Tag), ?LOG(Format, Args, ?alert, Tag)).
-define(ALERT(Format, Args),      ?ALERT(Format, Args, [])).
-define(ALERT(Format),            ?ALERT(Format, [])).

-define(EMERGENCY(Format, Args, Tag), ?LOG(Format, Args, ?emergency, Tag)).
-define(EMERGENCY(Format, Args),      ?EMERGENCY(Format, Args, [])).
-define(EMERGENCY(Format),            ?EMERGENCY(Format, [])).
