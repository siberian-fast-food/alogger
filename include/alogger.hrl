
-define(emergency, 0). % system is unusable
-define(alert,     1). % action must be taken immediately
-define(critical,  2). % critical conditions
-define(error,     3). % error conditions
-define(warning,   4). % warning conditions
-define(notice,    5). % normal but significant condition
-define(info,      6). % informational
-define(debug,     7). % debug-level messages

%% -record(lr, {tags   = [] :: list(),
%%              module      :: string(),
%%              format = "" :: string(),
%%              args   = [] :: list(),
%%              level,
%%              line,       :: non_neg_integer()
%%              pid          
%%             }
%%        ).


-define(LOG(Level, Tags, Format, Args),
        alog_if:log(Level, Tags, ?MODULE, ?LINE, self(), Format, Args)
       ).

%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-define(DBG(Tag, Format, Args), ?LOG(?debug, Tag, Format, Args)).
-define(DBG(Format, Args),      ?DBG([], Format, Args)).
-define(DBG(Format),            ?DBG(Format, [])).

%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-define(INFO(Tag, Format, Args), ?LOG(?info, Tag, Format, Args)).
-define(INFO(Format, Args),      ?INFO([], Format, Args)).
-define(INFO(Format),            ?INFO(Format, [])).

%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-define(NOTICE(Tag, Format, Args), ?LOG(?notice, Tag, Format, Args)).
-define(NOTICE(Format, Args),      ?NOTICE([], Format, Args)).
-define(NOTICE(Format),            ?NOTICE(Format, [])).

%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-define(WARNING(Tag, Format, Args), ?LOG(?warning, Tag, Format, Args)).
-define(WARNING(Format, Args),      ?WARNING([], Format, Args)).
-define(WARNING(Format),            ?WARNING(Format, [])).

%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-define(ERROR(Tag, Format, Args), ?LOG(?error, Tag, Format, Args)).
-define(ERROR(Format, Args),      ?ERROR([], Format, Args)).
-define(ERROR(Format),            ?ERROR(Format, [])).

%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-define(CRITICAL(Tag, Format, Args), ?LOG(?critical, Tag, Format, Args)).
-define(CRITICAL(Format, Args),      ?CRITICAL([], Format, Args)).
-define(CRITICAL(Format),            ?CRITICAL(Format, [])).

%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-define(ALERT(Tag, Format, Args), ?LOG(?alert, Tag, Format, Args)).
-define(ALERT(Format, Args),      ?ALERT([], Format, Args)).
-define(ALERT(Format),            ?ALERT(Format, [])).

%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-define(EMERGENCY(Tag, Format, Args), ?LOG(?emergency, Tag, Format, Args)).
-define(EMERGENCY(Format, Args),      ?EMERGENCY([], Format, Args)).
-define(EMERGENCY(Format),            ?EMERGENCY(Format, [])).

%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
