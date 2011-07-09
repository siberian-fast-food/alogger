-module(alog_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("alogger.hrl").

-define(all_prioritys, [
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
                  [?assertEqual(ok, priority_work(P)) || P <- ?all_prioritys]
              end)
      },
      {"priority barriers",
       ?_test([
               begin
                   ok = set_priority(CP),
                   GreatPriority = get_great_prioritys(CP),
                   [?assertEqual(error, priority_work(P)) || P <- GreatPriority],
                   LowPriority = get_low_prioritys(CP),
                   [?assertEqual(ok, priority_work(P)) || P <- [CP | LowPriority]]
               end
               || CP <- ?all_prioritys
              ])
      }
     ]
    }.

install_test_logger_iface() ->
    ok.

remove_test_logger_iface(State) ->
    ok.

get_great_prioritys(P) -> [Rp || Rp <- ?all_prioritys, Rp > P].
get_low_prioritys(P)   -> [Rp || Rp <- ?all_prioritys, Rp < P].
set_max_priority() -> set_priority(lists:max(?all_prioritys)).
set_priority(P)    -> ok. 

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
        {format, Ref} ->
            receive {log, Ref, Level} ->
                    ok
            after 100 ->
                    error
            end
    after 100 ->
            error
    end.
    

-endif.
