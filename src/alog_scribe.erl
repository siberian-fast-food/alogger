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
        , start/1
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

-include("alog.hrl").
-include("scribe_types.hrl").

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%----------------------------------------------------------------------
%%% @spec start(Opts::list()) -> ok
%%%
%%% @doc
%%% @end
%%%----------------------------------------------------------------------
-spec start(list()) -> ok.

start(Opts) ->
    SupRef = gen_alogger:get_opts(sup_ref, Opts),
    Pid = start_link(),
    attach_to_supervisor(SupRef, Pid),
    ok.

start() ->
    start_link(),
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
%%%              Module::atom(), Line::integer(), Pid::pid()) -> string()
%%%
%%% @doc returns formated log message
%%% @end
%%%----------------------------------------------------------------------
-spec format(string(), [term()], string(),
             atom(), integer(), pid()) -> string().

format(FormatString, Args, Tag, Module, Line, Pid) ->
    Msg = alog_common_formatter:format(FormatString, Args,
                                       Tag, Module, Line, Pid),
    lists:flatten(Msg).

%%=======================================================================
%% gen_server callbacks
%%=======================================================================
%%-----------------------------------------------------------------------
init([]) ->
    {ok, C} = thrift_client_util:new("localhost", 1463, scribe_thrift,
                                     [{strict_read, false}, 
                                      {strict_write, false}, 
                                      {framed, true}]),
    {ok, #state{connection=C}}.

%%-----------------------------------------------------------------------
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%%-----------------------------------------------------------------------
handle_cast({log, ScribePrio, Msg},
            #state{connection = Connection} = State) ->
    thrift_client:send_call(Connection,
                            'Log',
                            [[#logEntry{category = ScribePrio,
                                        message = Msg}]]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------------
handle_info(_Msg, StateData) ->
    {noreply, StateData}.

%%-----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%======================================================================
%%% Internal functions
%%%======================================================================
%%%----------------------------------------------------------------------
%%% @spec map_prio(ALoggerPrio::integer()) -> string()
%%%
%%% @doc maps alogger priorities to scribe priorities
%%% @end
%%%----------------------------------------------------------------------
-spec map_prio(integer()) -> string().

map_prio(?emergency) -> "emergency";
map_prio(?alert)     -> "alert";
map_prio(?critical)  -> "critical";
map_prio(?error)     -> "error";
map_prio(?warning)   -> "warning";
map_prio(?notice)    -> "notice";
map_prio(?info)      -> "info";
map_prio(?debug)     -> "debug".

attach_to_supervisor(_SupRef, _Pid) ->
    ok.
