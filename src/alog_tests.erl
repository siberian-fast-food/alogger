-module(alog_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("alog.hrl").

-define(all_priorities, [
                       ?emergency,
                       ?alert,
                       ?critical,
                       ?error,
                       ?warning,
                       ?notice,
                       ?info,
                       ?debug
                      ]).

base_test_() ->
    {setup,
     fun install_test_logger_iface/0,
     fun remove_test_logger_iface/1,
     [
      {"all priority",
       ?_test(begin
                  ok = set_max_priority(),
                  [?assertEqual(ok, priority_work(P)) || P <- ?all_priorities]
              end)
      },
      {"priority barriers",
       ?_test([
               begin
                   ok = set_priority(CP),
                   GreatPriority = get_great_priorities(CP),
                   [?assertEqual(error, priority_work(P)) || P <- GreatPriority],
                   LowPriority = get_low_priorities(CP),
                   [?assertEqual(ok, priority_work(P)) || P <- [CP | LowPriority]]
               end
               || CP <- ?all_priorities
              ])
      }
     ]
    }.

install_test_logger_iface() ->
    MaxPr = get_max_priotity(),
    alog_parse_trans:load_config(
      [{{mod,[?MODULE]}, {'=<', MaxPr},[alog_test_logger_iface]}]),
    ok.

remove_test_logger_iface(State) ->
    ok.

get_great_priorities(P) -> [Rp || Rp <- ?all_priorities, Rp > P].
get_low_priorities(P)   -> [Rp || Rp <- ?all_priorities, Rp < P].
get_max_priotity()     -> lists:max(?all_priorities).
set_max_priority() -> set_priority(get_max_priotity()).
set_priority(P)    ->
    alog_parse_trans:load_config(
      [{{mod,[?MODULE]}, {'=<', P},[alog_test_logger_iface]}]),
    ok.

priority_work(?debug)   ->
    Ref = make_ref(),
    ?DBG("test debug ~p", [Ref]),
    check_priority_work(?debug, Ref);
priority_work(?info)   ->
    Ref = make_ref(),
    ?INFO("test info ~p", [Ref]),
    check_priority_work(?info, Ref);
priority_work(?notice)   ->
    Ref = make_ref(),
    ?NOTICE("test notice ~p", [Ref]),
    check_priority_work(?notice, Ref);
priority_work(?warning)   ->
    Ref = make_ref(),
    ?WARNING("test warning ~p", [Ref]),
    check_priority_work(?warning, Ref);
priority_work(?error)   ->
    Ref = make_ref(),
    ?ERROR("test error ~p", [Ref]),
    check_priority_work(?error, Ref);
priority_work(?critical)   ->
    Ref = make_ref(),
    ?CRITICAL("test critical ~p", [Ref]),
    check_priority_work(?critical, Ref);
priority_work(?emergency)   ->
    Ref = make_ref(),
    ?EMERGENCY("test emergency ~p", [Ref]),
    check_priority_work(?emergency, Ref).

check_priority_work(Level, Ref) ->
    receive
        {format, Ref, _Tag, _Module, _Line, _Pid} ->
            receive {log, Level, Ref, _Tag, _Module, _Line, _Pid} ->
                    ok
            after 100 ->
                    error
            end
    after 100 ->
            error
    end.


-endif.
