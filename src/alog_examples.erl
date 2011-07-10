-module(alog_examples).

-include("alog.hrl").

-export([
         run_examples/0
        ]).

run_examples() ->
    io:format("*** all priorities~n", []),
    alog_parse_trans:load_config([{{mod,[?MODULE]}, {'=<', ?debug},[alog_tty]}]),
    
    TestVar = test_var_value,
    ?DBG("test debug ~p~n", [TestVar]),
    ?INFO("test info ~p~n", [TestVar]),
    ?NOTICE("test notice ~p~n", [TestVar]),
    ?WARNING("test warning ~p~n", [TestVar]),
    ?ERROR("test error ~p~n", [TestVar]),
    ?CRITICAL("test critical ~p~n", [TestVar]),
    ?EMERGENCY("test emergency ~p~n", [TestVar]),

    io:format("*** =< errors~n", []),

    alog_parse_trans:load_config(
      [{{mod,[?MODULE]}, {'=<', ?error},[alog_tty]}]),

    ?DBG("test debug ~p~n", [TestVar]),
    ?INFO("test info ~p~n", [TestVar]),
    ?NOTICE("test notice ~p~n", [TestVar]),
    ?WARNING("test warning ~p~n", [TestVar]),
    ?ERROR("test error ~p~n", [TestVar]),
    ?CRITICAL("test critical ~p~n", [TestVar]),
    ?EMERGENCY("test emergency ~p~n", [TestVar]),

    ok.
