%% @doc
%% This module contains examples of alogger usage.
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
                                   [{{alog_tty, alog_tty}, alog_common_formatter}]),

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
