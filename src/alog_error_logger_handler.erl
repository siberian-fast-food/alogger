%%%----------------------------------------------------------------------
%%% File    : alog_error_logger_handler.erl
%%% Author  : Igor Karymov <ingham.ka@gmail.com>
%%% Purpose :
%%% Created : 09 Jul 2011 by Igor Karymov <ingham.ka@gmail.com>
%%%
%%%
%%% alogger, Copyright (C) 2011  Siberian Fast Food
%%%----------------------------------------------------------------------

-module(alog_error_logger_handler).
-author('ingham.ka@gmail.com').

-export([ init/1
        , handle_event/2
        , handle_call/2
        , handle_info/2
        , terminate/2]).

-export([ install/0]).

-include("alog.hrl").

-define(ERROR_LOGGER_TAG, "$error_logger").

%%%----------------------------------------------------------------------
%%% @spec install() -> ok
%%%
%%% @doc installs new log handler
%%% @end
%%%----------------------------------------------------------------------
-spec install() -> ok.

install() ->
    gen_event:add_handler(error_logger, alog_error_logger_handler, []),
    ok.

%%%======================================================================
%%% callback functions
%%%======================================================================
init(_Args) ->
    {ok, []}.

handle_event(Event = {_ReportType, _, _}, State) ->
    process_event(Event),
    {ok, State};

handle_event(_Event , State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.

%%%======================================================================
%%% internal functions
%%%======================================================================
%%%----------------------------------------------------------------------
%%% @spec process_event(Event::tuple()) -> ok
%%%
%%% @doc
%%% @end
%%%----------------------------------------------------------------------
-spec process_event(tuple()) -> ok.

process_event(Event = {ReportType, _, _}) ->
    ALoggerPrio = map_prio(ReportType),
    alog_if_default:log(ALoggerPrio,       %% comments to be added...
                        ?ERROR_LOGGER_TAG, %%
                        "", "", "",        %%
                        Event,             %%
                        ""),               %%
    ok.

%%%----------------------------------------------------------------------
%%% @spec map_prio(ReportType::atom()) -> integer()
%%%
%%% @doc maps error_logger report types to alogger priorities
%%% @end
%%%----------------------------------------------------------------------
-spec map_prio(atom()) -> integer().

map_prio(error_repo)     -> ?error;
map_prio(error)          -> ?error;
map_prio(info_report)    -> ?info;
map_prio(info_msg)       -> ?info;
map_prio(info)           -> ?info;
map_prio(warning_report) -> ?warning;
map_prio(warning_msg)    -> ?warning.
