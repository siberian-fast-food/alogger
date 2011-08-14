%% @doc
%% The log interface towards disk_log logging facility
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

-module(alog_disk_log).
-behaviour(gen_alog).
-behaviour(gen_server).
-include_lib("alog.hrl").

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

-record(state, { log_ref
               , log_fun
               }).

-define(DEF_SUP_REF, alog_sup).

%% pre-defined key of 'format' option
-define(FORMAT_ARG, format).
%% default value for 'format' option
-define(FORMAT_DEF, internal).
%% pre-defined key for 'sync' option
-define(SYNC_ARG  , sync).
%% default value for 'sync' option
-define(SYNC_DEF  , false).
%% List of disk_log argumets
%% each item in list is in the following format:
%% {ConfigArg, DiskLogArg, DefaultValue}
-define(DEF_ARGS, [ {name       , name       , alogger_disk_log  }
                  , {file       , file       , "alogger_disk_log"}
                %%, {linkto     , linkto     , self()            }
                  , {repair     , repair     , false             }
                  , {type       , type       , halt              }
                  , {format     , ?FORMAT_ARG, ?FORMAT_DEF       }
                  , {size       , size       , infinity          }
                %%, {distributed, distributed, []                }
                  , {notify     , notify     , false             }
                %%, {head       , head       , none              }
                %%, {head_func  , head_func  , {M, F, A}         }
                  , {mode       , mode       , read_write        }
                  ]).

%% List of alog_disk_log specific arguments
%% each item in list is in the following format:
%% {ConfigArg, ADiskLogArg, DefaultValue}
-define(DEF_SPEC_ARGS, [ {sync       , ?SYNC_ARG  , ?SYNC_DEF}
                       , {single_file, single_file, true     }
                       ]).

%% Log functions mapping
%% the log function is selected depending on log format (internal | external)
%% and on sync option (true | false)
-define(LOG_FUNS, [ {internal, true , log  }
                  , {internal, false, alog }
                  , {external, true , blog }
                  , {external, false, balog}
                  ]).

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
  gen_server:call(?MODULE, stop).

%% @private
-spec log(integer(), string()) -> ok.
log(_ALoggerPrio, Msg) ->
  gen_server:cast(?MODULE, {log, "", Msg}),
  ok.

%%% gen_server callbacks
%% @private
init([Opts]) ->
  Args  = get_args(Opts),
  State = open_logs(Args),
  {ok, State}.

%% @private
handle_call(stop, _From, #state{log_ref = LogRef} = State) ->
  disk_log:sync(LogRef),
  disk_log:stop(LogRef),
  {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
  {reply, ok, State}.

%% @private
handle_cast({log, _, Msg},
            #state{ log_ref = LogRef
                  , log_fun = LogFun} = State) ->
  disk_log:LogFun(LogRef, Msg),
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

%% @private
get_args(Opts) ->
  DLArgs  = [{DiskLogArg , gen_alog:get_opt(ConfArg, Opts, DefVal)} ||
              {ConfArg, DiskLogArg , DefVal} <- ?DEF_ARGS],
  ADLArgs = [{ADiskLogArg, gen_alog:get_opt(ConfArg, Opts, DefVal)} ||
              {ConfArg, ADiskLogArg, DefVal} <- ?DEF_SPEC_ARGS],
  {DLArgs, ADLArgs}.

%% @private
open_logs({DLArgs, ADLArgs}) ->
  Format = gen_alog:get_opt(?FORMAT_ARG, DLArgs , ?FORMAT_DEF),
  Sync   = gen_alog:get_opt(?SYNC_ARG  , ADLArgs, ?SYNC_DEF),
  LogFun = get_log_fun(Format, Sync, ?LOG_FUNS),
  case disk_log:open(DLArgs) of
    {ok, LogRef} ->
      #state{ log_ref = LogRef
            , log_fun = LogFun
            };
    Error ->
      throw(Error)
  end.

%% @private
get_log_fun(Format, Sync, [{Format, Sync, Fun} | _])->
  Fun;
get_log_fun(Format, Sync, [_| T]) ->
  get_log_fun(Format, Sync, T).
