%% @doc
%% This module contains a handler for standart error_logger that allows
%% to log them too.
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

-module(alog_error_logger_handler).
-include_lib("alog.hrl").

%% API
-export([install/0]).
%% Callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2]).

-define(ERROR_LOGGER_TAG, '$error_logger').

%% @doc Installs new log handler
-spec install() -> ok.
install() ->
    gen_event:add_handler(error_logger, alog_error_logger_handler, []),
    ok.

%%% Callback functions
%% @private
init(_Args) ->
    {ok, []}.

%% @private
handle_event(Event = {_ReportType, _, _}, State) ->
    process_event(Event),
    {ok, State};

%% @private
handle_event(_Event , State) ->
    {ok, State}.

%% @private
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Args, _State) ->
    ok.

%%% internal functions
%% @private
-spec process_event(tuple()) -> ok.
process_event(Event = {ReportType, _, _}) ->
    ALoggerPrio = map_prio(ReportType),
    alog_if:log(Event,             %% this is a special case of
		[],                %% call alog_if:log.
		ALoggerPrio,       %% As FormatString we pass
		?ERROR_LOGGER_TAG, %% the error_logger event.
		"",                %% As Tag we pass a special
		"",                %% tag '$error_logger' which
		""),               %% will be understood by the
    ok.                            %% default formatter. The
                                   %% priority is mapped to the
                                   %% appropriate values. All
                                   %% other arguments are
                                   %% empty.

%% @private
%% @doc Maps error_logger report types to alogger priorities
-spec map_prio(atom()) -> integer().
map_prio(error_report)   -> ?error;
map_prio(error)          -> ?error;
map_prio(info_report)    -> ?info;
map_prio(info_msg)       -> ?info;
map_prio(info)           -> ?info;
map_prio(warning_report) -> ?warning;
map_prio(warning_msg)    -> ?warning.
