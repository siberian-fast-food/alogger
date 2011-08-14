-type filter() :: {mod, atom()} | {mod, [atom()]} |
                  {tag, atom()} | {tag, [atom()]} |
                  {app, atom()}.

-type logger() :: atom().

-type priority() :: debug | info | notice | warning | error 
                    | critical | alert | emergency | integer().

-type priority_expr() :: '<' | '>' | '=<' | '>=' | '==' | '/='.

-type priority_pattern() :: list({priority_expr(), priority()}) |
                            {priority_expr(), priority()} | priority().

-record(flow, {id              :: non_neg_integer(),
               filter          :: filter(),
               priority        :: priority_pattern(),
               formatter = alog_common_formatter :: module(),
               loggers   = []   :: list(logger()),
               enabled   = true :: true | {false, user} | {false, loggersOff}}).
