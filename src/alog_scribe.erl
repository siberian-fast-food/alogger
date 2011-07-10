%%%----------------------------------------------------------------------
%%% File    : alog_scribe.erl
%%% Author  : Alexander Dergachev <alexander.dergachev@gmail.com>
%%% Purpose :
%%% Created : 09 Jul 2011 by Alexander Dergachev
%%%                          <alexander.dergachev@gmail.com>
%%%
%%%
%%% alogger, Copyright (C) 2011  Siberian Fast Food
%%%----------------------------------------------------------------------

-module(alog_scribe).
-author('alexander.dergachev@gmail.com').

-behaviour(gen_alogger).
-behaviour(gen_server).

-export([ start/0
        , stop/0
        , log/2
        , format/6]).

-export([ start_link/0
	, init/1
	, handle_call/3
	, handle_cast/2
	, handle_info/2
	, terminate/2
	, code_change/3]).

-include("alogger.hrl").

-record(state, {connection}).

%%====================================================================
%% API
%%====================================================================
%%%----------------------------------------------------------------------
%%% @spec start_link() -> pid()
%%%
%%% @doc
%%% @end
%%%----------------------------------------------------------------------
-spec start_link() -> pid().

start_link() ->
    gen_server:start_link({local, ?MODULE}, [], []).

%%%----------------------------------------------------------------------
%%% @spec start() -> ok
%%%
%%% @doc
%%% @end
%%%----------------------------------------------------------------------
-spec start() -> ok.

start() ->
    ok.

%%%----------------------------------------------------------------------
%%% @spec stop() -> ok
%%%
%%% @doc
%%% @end
%%%----------------------------------------------------------------------
-spec start() -> ok.

stop() ->
    ok.

%%%----------------------------------------------------------------------
%%% @spec log(ALoggerPrio::integer(), Msg::string()) -> ok
%%%
%%% @doc logs message Msg with apropriate priority
%%% @end
%%%----------------------------------------------------------------------
-spec log(integer(), string()) -> ok.

log(ALoggerPrio, Msg) ->
    ScribePrio = map_prio(ALoggerPrio),
    gen_server:cast(?MODULE, {log, ScribePrio, Msg}),
    ok.

%%%----------------------------------------------------------------------
%%% @spec format(FormatString::string(), [term()], Tag::string(),
%%%              Module::atom(), Line::integer(), Pid::pid()) -> io_list()
%%%
%%% @doc returns formated log message
%%% @end
%%%----------------------------------------------------------------------
-spec format(string(), [term()], string(),
             atom(), integer(), pid()) -> iolist().

format(FormatString, Args, Tag, Module, Line, Pid) ->
    alog_common_formater:format(FormatString, Args,
                                Tag, Module, Line, Pid).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%%----------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%%----------------------------------------------------------------------
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info(_Msg, StateData) ->
    {noreply, StateData}.

%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
map_prio(?emergency) -> emergency;
map_prio(?alert)     -> alert;
map_prio(?critical)  -> critical;
map_prio(?error)     -> error;
map_prio(?warning)   -> warning;
map_prio(?notice)    -> notice;
map_prio(?info)      -> info;
map_prio(?debug)     -> debug.
