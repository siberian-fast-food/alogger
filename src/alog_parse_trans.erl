-module(alog_parse_trans).

-export([parse_transform/2]).

-define(?IFACE_MODE, alog_if).
-define(?IFACE_SOURCE, "alog_if.erl").

%---------------------------------
% Interfaces
%---------------------------------

% make alog_if_default.
parse_transform(Forms, _Opt) ->
    make_default_ast(Forms).


load_config(Config) ->
    case make_ast(Config) of
	{ok, NewAst} ->
	    {ok, ModuleName, Bin} = compile:forms(NewAst),
	    code:load_binary(ModuleName, ?IFACE_SOURCE, Bin); 
	Other ->
	    Other
    end.

% -------------------------------
% Internal functions
% -------------------------------
make_ast(Config) ->
    case check_config(Config) of 
	ok ->
	    make_proceed_ast(Config);
	Other ->
    end.

make_proceed_ast(Config) ->
    Clauses = multiply_clauses(Config),
    ChNameAst = change_module_name(alog_if_default:default_mod_ast()),
    insert_clauses(ChNameAst, Clauses).

% to make many many clauses)
multiply_clauses(Config) ->
    multiply_clauses(Config, [def_clause()]).
multiply_clauses([{{What,Mods},Prio, Loggers}|Configs], Acc) ->
    NewAcc = make_clause(What,Mods, Prio, Loggers, Acc),
    multiply_clauses(Configs, NewAcc);
multiply_clauses([], Acc) ->
    Acc.

make_clause(What, [Mod|Mods], {Guard,Pri}, Loggers, Acc)  ->
    AbsLogs = abstract(Loggers),
    make_clause(What,Mods, {Guard,Pri}, Loggers,[{clause, 0, get_arity(What,Mod),[get_guard(Guard, Pri)],AbsLogs}|Acc].   
make_clause(_,[], _, _, Acc) ->
    Acc

change_module_name(


% Check config. Modules are loaded?

check_config([{{mod,Mods},Prio, Loggers}|Configs]) ->
    case is_loaded(Mods, Prio, Loggers) of
	ok ->
	    check_config(Configs);
	Other ->
	    Other
    end;

check_config([{{tag,_},Prio, Loggers}|Configs]) ->
    case is_loaded(Prio, Loggers) of
	ok ->
	    check_config(Configs);
	Other ->
	    Other
    end;

check_config([]) ->
    ok;

% low check :-)

is_loaded(Mods, Prio, Loggers) ->
    ok.
is_loaded(Prio, Loggers) ->
    ok.


get_arity(mod,Mod) -> 
    [{var,0,'Level'},{atom,0,Mod},{var,0,'Tag'}].
get_arity(tag,Tag) -> 
    [{var,0,'Level'},{var,0,'Module'},{atom,0,Tag}].
get_guard(G, Level) ->
    [{op,0,G,{var,0,'Level'},{integer,0, Level}}].
    
def_clause() ->
    {function,_Line,default_modlogs_ast,_Arity,DefCl} = alog_if_default:default_modlogs_ast(),
    DefCl.
% --------------------------
make_default_ast(Forms) ->
    LogModAst = make_def_modlog_ast(Forms),
    ModAst    = abstract(LogModAst),
    mdma_transform(LogModAst, ModAst). 

% alog_if_default:default_logsmod_ast/1 should return AST of alog_if_default:get_logs_mod/3
make_def_modlog_ast(Forms) ->
    Glm = abstract(find_gml(Forms)),
    mdl_transform(Forms, Glm).  

find_gml([{function,Line,get_mod_logs,Arity,Clause}|_]) ->
    {function,Line,get_mod_logs,Arity,Clause};
find_gml([_|Fs]) ->
    find_gml(Fs);
find_gml([]) ->
    error("default interface is damaged").

% alog_if_default:default_mod_ast/1 should return AST of alog_if_default module
mdl_transform([F|Fs], Ast) ->
    F1 = mdl_transform_every(F, Ast),
    Fs1 = mdl_transform(Fs, Ast),
    [F1|Fs1];
mdl_transform([], _Ast) -> 
    [].

mdl_transform_every({function,Line,default_modlogs_ast,Arity,Clause}, Ast) ->
    NewClause = transform_clause(Clause, Ast),
    {function,Line,default_modlogs_ast,Arity,NewClause};
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

