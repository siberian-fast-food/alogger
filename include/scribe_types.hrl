-ifndef(_scribe_types_included).
-define(_scribe_types_included, yeah).

-define(scribe_fb_status_DEAD, 0).
-define(scribe_fb_status_STARTING, 1).
-define(scribe_fb_status_ALIVE, 2).
-define(scribe_fb_status_STOPPING, 3).
-define(scribe_fb_status_STOPPED, 4).
-define(scribe_fb_status_WARNING, 5).

-define(scribe_ResultCode_OK, 0).
-define(scribe_ResultCode_TRY_LATER, 1).

%% struct logEntry

-record(logEntry, {category = undefined :: string(), 
                   message = undefined :: string()}).

-endif.
