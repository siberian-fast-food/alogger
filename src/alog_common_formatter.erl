%% @doc
%% FIXME
%% @end
%% ----------------------------------------------------------------------
%% Copyright (c) 2011 Siberian Fast Food
%% Authors: Alexander Dergachev <alexander.dergachev@gmail.com>
%%          Artem Golovinsky    <artemgolovinsky@gmail.com>
%%          Igor Karymov        <ingham.k@gmail.com>
%%          Dmitry Groshev      <lambdadmitry@gmail.com>
%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% ----------------------------------------------------------------------

-module(alog_common_formatter).

-export([format/6]).

%% default log message format: module:line [pid]->[tag]: user message
-define(LOG_MSG_FORMAT, "~p:~p [~p]->[~p]: ~s~n").

%% @doc returns formated log message
-spec format(string(), [term()], string(),
             atom(), integer(), pid()) -> iolist().
%% this is a special case for error_logger messages
format(Msg, _, '$error_logger', _, _, _) ->
    write_report(Msg);
%% and this is for anything else
format(FormatString, Args, Tag, Module, Line, Pid) ->
    IoUserMsg = io_lib:format(FormatString, Args),
    io_lib:format(?LOG_MSG_FORMAT, [Module, Line, Pid,
                                    Tag, IoUserMsg]).

%%% internal functions
%% @private
-spec write_report(tuple()) -> iolist().
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

%% @private
-spec write_head(atom(), pid()) -> iolist().
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

%% @private
-spec write_head1(string(), pid()) -> iolist().
write_head1(Type, Pid) when node(Pid) /= node() ->
    io_lib:format("=~s==== (~p) ===~n",
    [Type, node(Pid)]);
write_head1(Type, _) ->
    io_lib:format("=~s=======~n",
    [Type]).

%% @private
-spec write_report2(iolist(), atom(), list()) -> iolist().
write_report2(Head, supervisor_report, Report) ->
    Name = sup_get(supervisor, Report),
    Context = sup_get(errorContext, Report),
    Reason = sup_get(reason, Report),
    Offender = sup_get(offender, Report),
    io_lib:format(Head ++ "     Supervisor: ~p~n"
                  "    Context:    ~p~n"
                  "    Reason:     ~80.18p~n"
                  "    Offender:   ~80.18p~n~n",
          [Name,Context,Reason,Offender]);
write_report2(Head, progress, Report) ->
    Format = format_key_val(Report),
    io_lib:format(Head ++ "~s", [Format]);
write_report2(Head, crash_report, Report) ->
    Format = proc_lib:format(Report),
    io_lib:format(Head ++ "~s", [Format]);
write_report2(Head, _Unknown, Report) ->
    io_lib:format(Head ++ "Unknown report: ~p", [Report]).

%% @private
-spec sup_get(atom(), list()) -> term().
sup_get(Tag, Report) ->
    case lists:keysearch(Tag, 1, Report) of
    {value, {_, Value}} ->
        Value;
    _ ->
        ""
    end.

%% @private
-spec format_key_val([{atom(), term()}]) -> iolist().
format_key_val([{Tag,Data}]) ->
    io_lib:format("    ~16w: ~p",[Tag,Data]) ++ format_key_val([]);
format_key_val([{Tag,Data}|Rep]) ->
    io_lib:format("    ~16w: ~p~n",[Tag,Data]) ++ format_key_val(Rep);
format_key_val(_) ->
    [].
