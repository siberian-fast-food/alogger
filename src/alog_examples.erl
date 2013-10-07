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
%% This module contains examples of alogger usage.
%% @end
%% ----------------------------------------------------------------------

-module(alog_examples).
-include_lib("alog_pt.hrl").
%-compile({parse_transform, alog_pt}).

-export([run_examples/0]).

%% @doc Shows some logger output.
-spec run_examples() -> ok.
run_examples() ->

    {ok, BackupFlows} = alog_control:get_flows(),
    ok = alog_control:delete_all_flows(),
    ok = alog_control:add_new_flow({mod,[?MODULE]}, {'=<', debug},
                                    [{{console_log, alog_tty}, alog_tty},
                                     {{syslog_log, alog_syslog}, alog_syslog}]),

    A = "foo",
    B = bar,

    io:format("*** all priorities~n", []),
    TestVar = test_var_value,
    ?DBG({A, B}),
    ?DBG({A, B, "testvar is ~p", foobar, [1, 2, 3]}, [TestVar]),
    ?DBG("test debug ~p", [TestVar]),
    ?INFO("test info ~p", [TestVar]),
    ?NOTICE("test notice ~p", [TestVar]),
    ?WARNING("test warning ~p", [TestVar]),
    ?ERROR("test error ~p", [TestVar]),
    ?CRITICAL("test critical ~p", [TestVar]),
    ?EMERGENCY("test emergency ~p", [TestVar]),

    io:format("*** =< errors~n", []),

    ok = alog_control:set_flow_priority(1, {'=<', error}),

    ?DBG("test debug ~p", [TestVar]),
    ?INFO("test info ~p", [TestVar]),
    ?NOTICE("test notice ~p", [TestVar]),
    ?WARNING("test warning ~p", [TestVar]),
    ?ERROR("test error ~p", [TestVar]),
    ?CRITICAL("test critical ~p", [TestVar]),
    ?EMERGENCY("test emergency ~p", [TestVar]),

    ok = alog_control:replace_flows(BackupFlows),
    ok.
