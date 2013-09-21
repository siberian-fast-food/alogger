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
%% @doc
%% The log interface towards disk_log logging facility
%% @end
%% ----------------------------------------------------------------------

-module(alog_disk_log).
-behaviour(gen_alog).
-behaviour(gen_server).
-include_lib("alog.hrl").
-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/2]).
%% gen_alog callbacks
-export([start/2,
         stop/2,
         log/3,
         format/8,
         reload/1]).
%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, { log_ref
               , log_fun
               , opts
               , name
               , inode
               , timer
               }).

-define(DEF_SUP_REF, alog_sup).

-define(CHECK_INTERVAL, 600000).

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
-spec start_link(atom(), list()) -> pid().
start_link(Name, Opts) ->
  OptsWithName = gen_alog:set_opt(name, Opts, Name),
  gen_server:start_link({local, Name}, ?MODULE, [OptsWithName], []).

%%% gen_alog callbacks
%% @private
-spec start(atom(), list()) -> ok.
start(Name, Opts) ->
  SupRef = gen_alog:get_opt(sup_ref, Opts, ?DEF_SUP_REF),
  attach_to_supervisor(SupRef, Name, Opts),
  ok.

%% @private
-spec stop(atom(), list()) -> ok.
stop(Name, _) ->
  gen_server:call(Name, stop).

%% @private
-spec log(atom(), integer(), string()) -> ok.
log(Name, _ALoggerPrio, Msg) ->
  gen_server:cast(Name, {log, "", Msg}),
  ok.

%% @private
%% @doc returns formated log message
-spec format(string(), [term()], integer(), list(),
             atom(), integer(), pid(),
             {non_neg_integer(), non_neg_integer(), non_neg_integer()}) -> iolist().
format(FormatString, Args, Level, Tag, Module, Line, Pid, TimeStamp) ->
  alog_common_formatter:format(FormatString, Args, Level,
                               Tag, Module, Line, Pid, TimeStamp).

-spec reload(atom()) -> ok.
reload(Name) ->
    gen_server:call(Name, reload).

%%% gen_server callbacks
%% @private
init([Opts]) ->
  Args  = get_args(Opts),
  {LogRef, LogFun} = open_logs(Args),
  Name = get_name(Args),
  Inode = get_inode(Name),
  Timer = erlang:start_timer(?CHECK_INTERVAL, self(), timeout),
  {ok, #state{log_ref = LogRef,
              log_fun = LogFun,
              opts = Opts,
              name = Name,
              inode = Inode,
              timer = Timer}}.

%% @private
handle_call(reload, _From, #state{log_ref = LogRef, opts = Opts, name = Name, timer = TimerRef} = State) ->
  erlang:cancel_timer(TimerRef),
  safe_close(LogRef),
  Args  = get_args(Opts),
  {LogRef1, LogFun1} = open_logs(Args),
  Inode = get_inode(Name),
  Timer = erlang:start_timer(?CHECK_INTERVAL, self(), timeout),
  {reply, ok, State#state{log_ref = LogRef1, log_fun = LogFun1, inode = Inode, timer = Timer}};
handle_call(stop, _From, #state{log_ref = LogRef} = State) ->
  safe_close(LogRef),
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
handle_info({timeout, TimerRef, _}, #state{timer = TimerRef,
                                           log_ref = LogRef,
                                           opts = Opts,
                                           name = Name,
                                           inode = Inode} = State) ->
  NewState =
    case get_inode(Name) of
      Inode ->
        State;
      _ ->
        safe_close(LogRef),
        Args  = get_args(Opts),
        {LogRef1, LogFun1} = open_logs(Args),
        NewInode = get_inode(Name),
        State#state{log_ref = LogRef1, log_fun = LogFun1, inode = NewInode}
    end,
  NewTimer = erlang:start_timer(?CHECK_INTERVAL, self(), timeout),
  {noreply, NewState#state{timer = NewTimer}};
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
attach_to_supervisor(SupRef, Name, Opts) ->
  Restart = permanent,
  Shutdown = 2000,
  ChildSpec = {Name,
               {?MODULE, start_link, [Name, Opts]},
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
      {LogRef, LogFun};
    {repaired, LogRef, _recovered, _badbytes} ->
      {LogRef, LogFun};
    Error ->
      throw(Error)
  end.

%% @private
get_log_fun(Format, Sync, [{Format, Sync, Fun} | _])->
  Fun;
get_log_fun(Format, Sync, [_| T]) ->
  get_log_fun(Format, Sync, T).


get_name({DLArgs, _ADLArgs}) ->
  {file, Name} = proplists:lookup(file, DLArgs),
  Name.

get_inode(Name) ->
  case file:read_file_info(Name) of
    {ok, Info} ->
      Info#file_info.inode;
    _ ->
      undefined
  end.

safe_close(LogRef) ->
  disk_log:sync(LogRef),
  disk_log:close(LogRef).
