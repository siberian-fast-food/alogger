%% @doc
%% The log interface towards scribe log daemon through thrift protocol.
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

-module(alog_scribe).
-behaviour(gen_alog).
-behaviour(gen_server).
-include_lib("alog.hrl").
-include_lib("scribe_types.hrl").

%% API
-export([start_link/1]).
%% gen_alog callbacks
-export([start/1,
         stop/1,
         log/2]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {connection}).
-define(DEF_SUP_REF, alog_sup).

-define(DEF_ADDR, "localhost").
-define(DEF_PORT, 1463).
-define(DEF_STRICT_READ, false).
-define(DEF_STRICT_WRITE, false).
-define(DEF_FRAMED, true).

%%% API
%% @doc Starts logger
-spec start_link(list()) -> pid().
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

%%% gen_alog callbacks
%% @private
-spec start(list()) -> ok.
start(Opts) ->
    SupRef = gen_alog:get_opt(sup_ref, Opts, ?DEF_SUP_REF),
    attach_to_supervisor(SupRef, Opts),
    ok.

%% @private
-spec stop(list()) -> ok.
stop(_) ->
    ok.

%% @private
-spec log(integer(), string()) -> ok.
log(ALoggerPrio, Msg) ->
    ScribePrio = map_prio(ALoggerPrio),
    gen_server:cast(?MODULE, {log, ScribePrio, lists:flatten(Msg)}),
    ok.

%%% gen_server callbacks
%% @private
init([Opts]) ->
    Addr = gen_alog:get_opt(addr, Opts, ?DEF_ADDR),
    Port = gen_alog:get_opt(port, Opts, ?DEF_PORT),
    StrictRead = gen_alog:get_opt(strict_read, Opts, ?DEF_STRICT_READ),
    StrictWrite = gen_alog:get_opt(strict_write, Opts, ?DEF_STRICT_WRITE),
    Framed = gen_alog:get_opt(framed, Opts, ?DEF_FRAMED),
    {ok, C} = thrift_client_util:new(Addr, Port, scribe_thrift,
                                     [{strict_read, StrictRead},
                                      {strict_write, StrictWrite},
                                      {framed, Framed}]),
    {ok, #state{connection=C}}.

%% @private
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% @private
handle_cast({log, ScribePrio, Msg},
            #state{connection = Connection} = State) ->
    thrift_client:send_call(Connection,
                            'Log',
                            [[#logEntry{category = ScribePrio,
                                        message = Msg}]]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Msg, StateData) ->
    {noreply, StateData}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
%% @private
%% @doc Maps alogger priorities to scribe priorities
-spec map_prio(integer()) -> string().
map_prio(?emergency) -> "emergency";
map_prio(?alert)     -> "alert";
map_prio(?critical)  -> "critical";
map_prio(?error)     -> "error";
map_prio(?warning)   -> "warning";
map_prio(?notice)    -> "notice";
map_prio(?info)      -> "info";
map_prio(?debug)     -> "debug".

%% @private
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
