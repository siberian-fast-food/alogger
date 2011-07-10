-module(alog_examples).

-include("alog.hrl").
-compile({parse_transform, alog_pt}).

-export([
         run_examples/0
        ]).

run_examples() ->
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

    %% alog_parse_trans:load_config(
    %%   [{{mod,[?MODULE]}, {'=<', ?error},[alog_tty]}]),

    ?DBG("test debug ~p", [TestVar]),
    ?INFO("test info ~p", [TestVar]),
    ?NOTICE("test notice ~p", [TestVar]),
    ?WARNING("test warning ~p", [TestVar]),
    ?ERROR("test error ~p", [TestVar]),
    ?CRITICAL("test critical ~p", [TestVar]),
    ?EMERGENCY("test emergency ~p", [TestVar]),

    ok.
