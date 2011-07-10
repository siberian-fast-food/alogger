-module(alog_examples).

-include("alog.hrl").
-compile({parse_transform, alog_pt}).

-export([
         run_examples/0
        ]).

run_examples() ->

    {ok, BackupFlows} = alog_control:get_flows(),
    ok = alog_control:delete_all_flows(),
    ok = alog_control:add_new_flow({mod,[?MODULE]}, {'>=', debug}, [alog_tty]),
    
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

    ok = alog_control:delete_all_flows(),
    [ok = alog_control:add_new_flow(F, P, L) ||
        {flow, _Id, F, P, L, _E} <- BackupFlows],
    ok.
