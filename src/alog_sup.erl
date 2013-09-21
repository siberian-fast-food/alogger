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
%% @private
%% @doc
%% Main alogger supervisor.
%% @end
%% ----------------------------------------------------------------------

-module(alog_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%% API functions
%% @doc Starts the supervisor
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%% Supervisor callbacks
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,

    Control =
        {alog_control, {alog_control, start_link, []},
         Restart, Shutdown, worker, [alog_control]},

    {ok,
     {SupFlags, [Control]
     }}.
