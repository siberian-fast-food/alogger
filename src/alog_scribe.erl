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

-export([ start/1
        , stop/1
        , log/2
        , format/6]).

-export([ start_link/1
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

-include("alog.hrl").
-include("scribe_types.hrl").

-record(state, {connection}).
-define(DEF_SUP_REF, alog_sup).

-define(DEF_ADDR, "localhost").
-define(DEF_PORT, 1463).
-define(DEF_STRICT_READ, false).
-define(DEF_STRICT_WRITE, false).
-define(DEF_FRAMED, true).


%%====================================================================
%% API
%%====================================================================
%%%----------------------------------------------------------------------
%%% @spec start_link(Opts::list()) -> pid()
%%%
%%% @doc
%%% @end
%%%----------------------------------------------------------------------
-spec start_link(list()) -> pid().

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

%%%----------------------------------------------------------------------
%%% @spec start(Opts::list()) -> ok
%%%
%%% @doc
%%% @end
%%%----------------------------------------------------------------------
-spec start(list()) -> ok.

start(Opts) ->
    SupRef = gen_alogger:get_opt(sup_ref, Opts, ?DEF_SUP_REF),
    attach_to_supervisor(SupRef, Opts),
    ok.

%%%----------------------------------------------------------------------
%%% @spec stop(Opts::list()) -> ok
%%%
%%% @doc
%%% @end
%%%----------------------------------------------------------------------
-spec stop(list()) -> ok.

stop(_) ->
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
init([Opts]) ->
    Addr = gen_alogger:get_opt(addr, Opts, ?DEF_ADDR),
    Port = gen_alogger:get_opt(port, Opts, ?DEF_PORT),
    StrictRead = gen_alogger:get_opt(strict_read, Opts, ?DEF_STRICT_READ),
    StrictWrite = gen_alogger:get_opt(strict_write, Opts, ?DEF_STRICT_WRITE),
    Framed = gen_alogger:get_opt(framed, Opts, ?DEF_FRAMED),
    {ok, C} = thrift_client_util:new(Addr, Port, scribe_thrift,
                                     [{strict_read, StrictRead},
                                      {strict_write, StrictWrite},
                                      {framed, Framed}]),
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

attach_to_supervisor(SupRef, Opts) ->
    Restart = permanent,
    Shutdown = 2000,
    ChildSpec = {?MODULE,
		 {?MODULE, start_link, [Opts]},
		 Restart,
		 Shutdown,
		 worker,
		 [?MODULE]},
    supervisor:start_child(SupRef, ChildSpec),
    ok.
