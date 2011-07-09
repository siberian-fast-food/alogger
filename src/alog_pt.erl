-module(alog_pt).
-include_lib("alogger.hrl").
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

%% Finds and replaces function that
-spec replace_logfun(atom(), stxtree(), _, list()) -> {stxtree(), list()}.
replace_logfun(application, Form, _Ctxt, Acc) ->
    MFA = erl_syntax_lib:analyze_application(Form),
    case MFA of
        {?LOGMOD, {?LOGFUN, 1}} ->
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
    VarsT = erl_syntax:list([erl_syntax:variable(X) || X <- Vars]),
    FunT = erl_syntax:atom(format),
    ModT = erl_syntax:atom(io_lib),
    OperatorT = erl_syntax:module_qualifier(ModT, FunT),
    erl_syntax:application(OperatorT, [FStrT, VarsT]).

-spec (elems_folder(stxtree(), Acc) ->
              Acc when Acc :: {list(nonempty_string()), list(atom())}).
elems_folder(Elem, {FStrs, Vars}) ->
    case erl_syntax:type(Elem) of
        variable ->
            FStr = erl_syntax:variable_literal(Elem) ++ ": ~p",
            Var = erl_syntax:variable_name(Elem),
            {[FStr|FStrs], [Var|Vars]};
        string ->
            Str = erl_syntax:string_value(Elem),
            {[Str|FStrs], Vars};
        T ->
            parse_trans:error(badarg_in_tupleexpr,
                              ?LINE, [{type, T}])
    end.
