-module(alog_examples).

-include("alog.hrl").

-export([
         run_examples/0
        ]).

run_examples() ->
    io:format("*** all priorities~n", []),
    alog_parse_trans:load_config([{{mod,[?MODULE]}, {'=<', ?debug},[alog_tty]}]),
    
    TestVar = test_var_value,
    ?DBG("test debug ~p", [TestVar]),
    ?INFO("test info ~p", [TestVar]),
    ?NOTICE("test notice ~p", [TestVar]),
    ?WARNING("test warning ~p", [TestVar]),
    ?ERROR("test error ~p", [TestVar]),
    ?CRITICAL("test critical ~p", [TestVar]),
    ?EMERGENCY("test emergency ~p", [TestVar]),

    io:format("*** =< errors~n", []),

    alog_parse_trans:load_config(
      [{{mod,[?MODULE]}, {'=<', ?error},[alog_tty]}]),

    ?DBG("test debug ~p", [TestVar]),
    ?INFO("test info ~p", [TestVar]),
    ?NOTICE("test notice ~p", [TestVar]),
    ?WARNING("test warning ~p", [TestVar]),
    ?ERROR("test error ~p", [TestVar]),
    ?CRITICAL("test critical ~p", [TestVar]),
    ?EMERGENCY("test emergency ~p", [TestVar]),

    ok.
