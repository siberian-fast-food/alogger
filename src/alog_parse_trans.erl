-module(alog_parse_trans).

-export([parse_transform/2, load_config/1]).

-include("alogger.hrl").
-define(IFACE_MODE, alog_if).
-define(IFACE_SOURCE, "alog_if.erl").

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
	    Other
    end.

make_proceed_ast(Config) ->
    Clauses = multiply_clauses(Config),
    DefAst = alog_if_default:default_mod_ast(),
    insert_clauses(DefAst, Clauses).

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
    make_clause(What,Mods, {Guard,Pri}, Loggers,[{clause, 0, get_arity(What,Mod),[get_guard(Guard, Pri)],AbsLogs}|Acc]);   
make_clause(_,[], _, _, Acc) ->
    Acc.

% insert all clauses to get_log_mods/3
insert_clauses([F|Fs], Clauses) ->
    F1 = insert_clauses_every(F, Clauses),
    Fs1 = insert_clauses(Fs, Clauses),
    [F1|Fs1];
insert_clauses([], _Ast) ->
    [].

insert_clauses_every({function,Line,get_mod_logs,Arity,_Clause}, Clauses) ->
    {function,Line,get_mod_logs,Arity,Clauses};    
insert_clauses_every(Any, _Clauses) ->
    Any.

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
    ok.

% low check :-)

is_loaded(Mods, Prio, Loggers) ->
    is_loaded(Prio, Mods ++ Loggers).

is_loaded(Prio, AllMods) ->
    Loaded = lists:filter(fun(Elem) ->
				  case code:is_loaded(Elem) of
				      {file, _} ->
					  false;
				      false ->
					  true
				  end
			   end, AllMods),
    
    case Loaded of
	[] ->
	    check_prio(Prio) ;
	Other ->
	    {error,modules_not_loaded, Other}
    end.

check_prio({'>', Level}) when Level >= ?emergency, Level =< ?debug ->
    ok;
check_prio({'<', Level}) when Level >= ?emergency, Level =< ?debug ->
    ok;
check_prio(Other) ->
    {error, wrong_level, Other}.

get_arity(mod,Mod) -> 
    [{var,0,'Level'},{atom,0,Mod},{var,0,'Tag'}];
get_arity(tag,Tag) -> 
    [{var,0,'Level'},{var,0,'Module'},{atom,0,Tag}].
get_guard(G, Level) ->
    [{op,0,G,{var,0,'Level'},{integer,0, Level}}].
    
def_clause() ->
    {function,_Line,get_mod_logs,_Arity,DefCl} = alog_if_default:default_modlogs_ast(),
    DefCl.
% --------------------------
make_default_ast(Forms) ->
%    LogModAst = make_def_modlog_ast(Forms),
%    io:format("LogModAst ~p ~n",[LogModAst]),
    ModAst    = abstract(change_and_remove(Forms)),
    Glm = abstract(find_gml(Forms)),
    mdma_transform(Forms, ModAst, Glm). 

find_gml([{function,Line,get_mod_logs,Arity,Clause}|_]) ->
    {function,Line,get_mod_logs,Arity,Clause};
find_gml([_|Fs]) ->
    find_gml(Fs);
find_gml([]) ->
    error("default interface is damaged").

% -----------------------------------

mdma_transform([F|Fs], Ast, Glm) ->
    F1 = mdma_transform_every(F, Ast, Glm),
    Fs1 = mdma_transform(Fs, Ast, Glm),
    [F1|Fs1];
mdma_transform([], _Ast, _Glm) -> [].
  
mdma_transform_every({function,Line,default_mod_ast,Arity,Clause}, Ast, _Glm) ->
    NewClause = transform_clause(Clause, Ast),
    {function,Line,default_mod_ast,Arity,NewClause};

mdma_transform_every({function,Line,default_modlogs_ast,Arity,Clause}, _Ast, Glm) ->
    NewClause = transform_clause(Clause, Glm),
    {function,Line,default_modlogs_ast,Arity,NewClause};

mdma_transform_every(Node, _Ast, _Glm) ->
    Node.
% ------------------------------------

change_and_remove([{function,_,default_modlogs_ast,_,_}|Fs]) ->
    Fs1 = change_and_remove(Fs),
    Fs1;    
change_and_remove([{function,_,default_mod_ast,_,_}|Fs]) ->
    Fs1 = change_and_remove(Fs),
    Fs1; 
change_and_remove([F|Fs]) ->
    F1 = cmd_every(F),
    Fs1 = change_and_remove(Fs),
    [F1|Fs1];
change_and_remove([]) -> [].

cmd_every({attribute,_,file,{_,_}}) ->
    {attribute,1,file,{?IFACE_SOURCE,1}};
cmd_every({attribute,_,module,_}) ->
    {attribute,1,module,?IFACE_MODE};
cmd_every(Node) ->
    Node.
%---------------------------------------
transform_clause([{clause, Line, Op, Guard, _Result}|_],Ast) ->
    [{clause, Line, Op, Guard, [Ast]}].

abstract(Term) ->
    erl_syntax:revert(erl_syntax:abstract(Term)).

