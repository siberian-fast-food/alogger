-module(alog_pt).
-include_lib("alog.hrl").
-export([parse_transform/2]).

-type stxtree() :: tuple().
-type forms()   :: [stxtree()].
-type options() :: [{atom(), any()}].

%% This API function makes actual parse transformation
-spec parse_transform(forms(), options()) -> forms().
parse_transform(Forms, Opts) ->
    {NewForms, _} = parse_trans:depth_first(fun replace_logfun/4, [],
                                            Forms, Opts),
    parse_trans:revert(NewForms).

%% Finds and replaces function that is defined as logging function in
%% alog
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

-spec update_args([stxtree(), ...]) -> [stxtree(), ...].
update_args([Format|Args]) ->
    Format2 = case erl_syntax:type(Format) of
                  tuple ->
                      build_format(Format);
                  _ -> Format
              end,
    [Format2|Args].

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
                                     erl_syntax:string(X)
                             end || X <- Vars]),
    IoFormatT = build_application(io_lib, format, [FStrT, VarsT]),
    build_application(lists, flatten, [IoFormatT]).

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
        T ->
            parse_trans:error(badarg_in_tupleexpr,
                              ?LINE, [{type, T}])
    end.

-spec build_application(atom(), atom(), [stxtree()]) -> stxtree().
build_application(Mod, Fun, Args) ->
    ModT = erl_syntax:atom(Mod),
    FunT = erl_syntax:atom(Fun),
    OperatorT = erl_syntax:module_qualifier(ModT, FunT),
    erl_syntax:application(OperatorT, Args).
