-module(alog_tty).
-author('alexander.dergachev@gmail.com').

-behaviour(gen_alogger).

-export([ start/1
        , stop/1
        , log/2
        , format/6]).

%%%----------------------------------------------------------------------
%%% @spec start(Opts::list()) -> ok
%%%
%%% @doc
%%% @end
%%%----------------------------------------------------------------------
-spec start(list()) -> ok.

start(Options) ->
    io:format("start alog_tty with options ~p", [Options]),
    ok.

%%%----------------------------------------------------------------------
%%% @spec stop(Opts::list()) -> ok
%%%
%%% @doc
%%% @end
%%%----------------------------------------------------------------------
-spec stop(list()) -> ok.

stop(_) ->
    ok.

%%%----------------------------------------------------------------------
%%% @spec log(ALoggerPrio::integer(), Msg::string()) -> ok
%%%
%%% @doc
%%% @end
%%%----------------------------------------------------------------------
-spec log(integer(), string()) -> ok.

log(_ALoggerPrio, Msg) ->
    io:format("~s", [Msg]).

%%%----------------------------------------------------------------------
%%% @spec format(FormatString::string(), [term()], Tag::string(),
%%%              Module::atom(), Line::integer(), Pid::pid()) -> io_list()
%%%
%%% @doc returns formated log message
%%% @end
%%%----------------------------------------------------------------------
-spec format(string(), [term()], string(),
             atom(), integer(), pid()) -> iolist().

format(FormatString, Args, Tag, Module, Line, Pid) ->
    alog_common_formatter:format(FormatString, Args,
                                Tag, Module, Line, Pid).
