-module(alog_tty).
-author('alexander.dergachev@gmail.com').

-behaviour(gen_alogger).

-export([ start/1
        , stop/0
        , log/2
        , format/6]).

%%%----------------------------------------------------------------------
%%% @spec start(SupRef::term()) -> ok
%%%
%%% @doc
%%% @end
%%%----------------------------------------------------------------------
-spec start(term()) -> ok.

start(_) ->
    ok.

%%%----------------------------------------------------------------------
%%% @spec stop() -> ok
%%%
%%% @doc
%%% @end
%%%----------------------------------------------------------------------
-spec stop() -> ok.

stop() ->
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
    alog_common_formater:format(FormatString, Args,
                                Tag, Module, Line, Pid).
