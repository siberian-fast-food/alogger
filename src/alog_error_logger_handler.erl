-module(alog_error_logger_handler).

-export([
         init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2
        ]).

-export([
         install/0
        ]).

install() ->
    gen_event:add_handler(error_logger, alog_error_logger_handler, []),
    ok.

init(_Args) ->
    {ok, []}.

handle_event(Event = {ReportType, _, _}, State) ->
    io:format("handle event ~p", [Event]),
    {ok, State};

handle_event(Event , State) ->
    io:format("unhandled event ~p", [Event]),
    {ok, State}.

handle_call(_Request, State) ->
    io:format("error_logger_handler: Unhandled call!!! ~p~n", [_Request]),
    {ok, ok, State}.

handle_info(_Info, State) -> 
    io:format("error_logger_handler: Unhandled info!!! ~p~n", [_Info]),
    {ok, State}.

terminate(_Args, _State) ->
    io:format("*** error_logger_handler terminates!!!~p ~p~n", [_Args, _State]),
    ok.

%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_report({error_report, _GL, {Pid, Type, Report}}) ->
    Head = write_head(Type, Pid),
    write_report2(Head, Type, Report);

write_report({info_report, _GL, {Pid, Type, Report}}) ->
    Head = write_head(Type, Pid),
    write_report2(Head, Type, Report);

write_report({warning_report, _GL, {Pid, Type, Report}}) ->
    Head = write_head(Type, Pid),
    write_report2(Head, Type, Report);

write_report({error, _GL, {_Pid, Format, Args}}) ->
    Head = write_head(error_report, _Pid),
    io_lib:format(Head ++ Format,Args);

write_report({info_msg, _GL, {_Pid, Format, Args}}) ->
    io_lib:format(Format,Args);

write_report({warning_msg, _GL, {_Pid, Format, Args}}) ->
    io_lib:format(Format,Args);

write_report(Event) ->
    io_lib:format("Bad formated error_logger event ~p", [Event]).

write_head(supervisor_report, Pid) ->
    write_head1("SUPERVISOR REPORT", Pid);
write_head(crash_report, Pid) ->
    write_head1("CRASH REPORT", Pid);
write_head(progress, Pid) ->
    write_head1("PROGRESS REPORT", Pid);
write_head(error_report, Pid) ->
    write_head1("ERROR REPORT", Pid);
write_head(std_info, Pid) ->
    write_head1("STD INFO", Pid);
write_head(std_error, Pid) ->
    write_head1("STD ERROR", Pid);
write_head(_Unknown, Pid) ->
    write_head1("UNKNOWN", Pid).

write_head1(Type, Pid) when node(Pid) /= node() ->
    io_lib:format("=~s==== (~p) ===~n",
    [Type, node(Pid)]);
write_head1(Type, _) ->
    io_lib:format("=~s=======~n",
    [Type]).

write_report2(Head, supervisor_report, Report) ->
    Name = sup_get(supervisor, Report),
    Context = sup_get(errorContext, Report),
    Reason = sup_get(reason, Report),
    Offender = sup_get(offender, Report),
    io_lib:format(Head ++ "     Supervisor: ~p~n     Context:    ~p~n     Reason:     "
          "~80.18p~n     Offender:   ~80.18p~n~n",
          [Name,Context,Reason,Offender]);
write_report2(Head, progress, Report) ->
    Format = format_key_val(Report),
    io_lib:format(Head ++ "~s", [Format]);
write_report2(Head, crash_report, Report) ->
    Format = proc_lib:format(Report),
    io_lib:format(Head ++ "~s", [Format]);
write_report2(Head, _Unknown, Report) ->
    io_lib:format(Head ++ "Unknown report: ~p", [Report]).

sup_get(Tag, Report) ->
    case lists:keysearch(Tag, 1, Report) of
    {value, {_, Value}} ->
        Value;
    _ ->
        ""
    end.

format_key_val([{Tag,Data}]) ->
    io_lib:format("    ~16w: ~p",[Tag,Data]) ++ format_key_val([]);
format_key_val([{Tag,Data}|Rep]) ->
    io_lib:format("    ~16w: ~p~n",[Tag,Data]) ++ format_key_val(Rep);
format_key_val(_) ->
    [].
