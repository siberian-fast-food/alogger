%% @doc
%% This module contains parse_transform to give user an ability to use
%% "tuple expressions": ?DBG({A, "string",  B}) will log string
%% "A: [A value] string B: [B value]".
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

-module(alog_pt).
-include_lib("alog.hrl").
-export([parse_transform/2]).

-type stxtree() :: tuple().
-type forms()   :: [stxtree()].
-type options() :: [{atom(), any()}].

%% @doc This API function makes actual parse transformation
-spec parse_transform(forms(), options()) -> forms().
parse_transform(Forms, Opts) ->
    {NewForms, _} = parse_trans:depth_first(fun replace_logfun/4, [],
                                            Forms, Opts),
    parse_trans:revert(NewForms).

%% @private
%% @doc Finds and replaces function that is defined as logging function in
%% alog.hrl (by ?LOGMOD/?LOGFUN macroses)
-spec replace_logfun(atom(), stxtree(), _, list()) -> {stxtree(), list()}.
replace_logfun(application, Form, _Ctxt, Acc) ->
    MFA = erl_syntax_lib:analyze_application(Form),
    case MFA of
        {?LOGMOD, {?LOGFUN, _}} ->
            Args = erl_syntax:application_arguments(Form),
            NewArgs = update_args(Args),
            FunT = erl_syntax:atom(?LOGFUN),
            ModT = erl_syntax:atom(?LOGMOD),
            OperatorT = erl_syntax:module_qualifier(ModT, FunT),
            NewForm = erl_syntax:application(OperatorT, NewArgs),
            {NewForm, Acc};
        _ ->
            {Form, Acc}
    end;
replace_logfun(_, Form, _Ctxt, Acc) ->
    {Form, Acc}.

%% @private
%% @doc Updates arguments of logging function (in fact, just first
%% argument)
-spec update_args([stxtree(), ...]) -> [stxtree(), ...].
update_args([Format|Args]) ->
    Format2 = case erl_syntax:type(Format) of
                  tuple ->
                      build_format(Format);
                  _ -> Format
              end,
    [Format2|Args].

%% @private
%% @doc Builds io_lib:format() and constructs its arguments
-spec build_format(stxtree()) -> stxtree().
build_format(Format) ->
    Elems = erl_syntax:tuple_elements(Format),
    {FStrsR, VarsR} = lists:foldl(fun elems_folder/2, {[], []}, Elems),
    FStr = string:join(lists:reverse(FStrsR), " "),
    Vars = lists:reverse(VarsR),

    FStrT = erl_syntax:string(FStr),
    VarsT = erl_syntax:list([case X of
                                 X when is_atom(X) ->
                                     erl_syntax:variable(X);
                                 X when is_list(X) ->
                                     erl_syntax:string(X);
                                 X -> X
                             end || X <- Vars]),
    IoFormatT = build_application(io_lib, format, [FStrT, VarsT]),
    build_application(lists, flatten, [IoFormatT]).

%% @private
%% This functions is used in fold in build_format. It accumulates variables
%% and strings that are used in tuple expression
-spec (elems_folder(stxtree(), Acc) ->
              Acc when Acc :: {list(nonempty_string()), list(atom())}).
elems_folder(Elem, {FStrs, Vars}) ->
    case erl_syntax:type(Elem) of
        variable ->
            FStr = erl_syntax:variable_literal(Elem) ++ ": ~p",
            Var = erl_syntax:variable_name(Elem),
            {[FStr|FStrs], [Var|Vars]};
        string ->
            FStr = "~s",
            Str = erl_syntax:string_value(Elem),
            {[FStr|FStrs], [Str|Vars]};
        _ ->
            FStr = "~p",
            {[FStr|FStrs], [Elem|Vars]}
    end.

%% @private
%% Builds application of M:F function to Args
-spec build_application(atom(), atom(), [stxtree()]) -> stxtree().
build_application(Mod, Fun, Args) ->
    ModT = erl_syntax:atom(Mod),
    FunT = erl_syntax:atom(Fun),
    OperatorT = erl_syntax:module_qualifier(ModT, FunT),
    erl_syntax:application(OperatorT, Args).
