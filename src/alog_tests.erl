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

base_test() ->
    {setup,
     fun install_test_logger_iface/0,
     fun remove_test_logger_iface/0,
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

remove_test_logger_iface() ->
    ok.

get_great_prioritys(P) -> [Rp || Rp <- ?all_prioritys, Rp > P].
get_low_prioritys(P)   -> [Rp || Rp <- ?all_prioritys, Rp < P].
set_max_priority() -> ok.
set_priority(P)    -> ok. 
priority_work(P)   -> ok.
    

-endif.
