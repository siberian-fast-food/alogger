-module(alog_parse_trans).

-export([parse_transform/2]).

% make alog_if_default.
parse_transform(Forms, _Opt) ->
    make_default_ast(Forms).

make_default_ast(Forms) ->
    LogModAst = make_def_logmod_ast(Forms),
    ModAst    = abstract(LogModAst),
    mdma_transform(LogModAst, ModAst). 

% alog_if_default:default_logsmod_ast/1 should return AST of alog_if_default:get_logs_mod/3
make_def_logmod_ast(Forms) ->
    Glm = abstract(find_glm(Forms)),
    mdl_transform(Forms, Glm).  

find_glm([{function,Line,get_logs_mod,Arity,Clause}|_]) ->
    {function,Line,get_logs_mod,Arity,Clause};

find_glm([_|Fs]) ->
    find_glm(Fs);

find_glm([]) ->
    error("default interface is damaged").

% alog_if_default:default_mod_ast/1 should return AST of alog_if_default module
mdl_transform([F|Fs], Ast) ->
    F1 = mdl_transform_every(F, Ast),
    Fs1 = mdl_transform(Fs, Ast),
    [F1|Fs1];

mdl_transform([], _Ast) -> [].


mdl_transform_every({function,Line,default_logsmod_ast,Arity,Clause}, Ast) ->
    NewClause = transform_clause(Clause, Ast),
    {function,Line,default_logsmod_ast,Arity,NewClause};

mdl_transform_every(Node, _Ast) ->
    Node.

mdma_transform([F|Fs], Ast) ->
    F1 = mdma_transform_every(F, Ast),
    Fs1 = mdma_transform(Fs, Ast),
    [F1|Fs1];

mdma_transform([], _Ast) -> [].

mdma_transform_every({function,Line,default_mod_ast,Arity,Clause}, Ast) ->
    NewClause = transform_clause(Clause, Ast),
    {function,Line,default_mod_ast,Arity,NewClause};

mdma_transform_every(Node, _Ast) ->
    Node.

transform_clause([{clause, Line, Op, Guard, _Result}|_],Ast) ->
    [{clause, Line, Op, Guard, [Ast]}].

abstract(Term) ->
    erl_syntax:revert(erl_syntax:abstract(Term)).

