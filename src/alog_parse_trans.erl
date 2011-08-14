%% @doc
%% This module contains parse transformer that creates an actual
%% logging module.
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

-module(alog_parse_trans).

-export([parse_transform/2, load_config/1]).

-include("alog.hrl").
-include("alog_flow.hrl").
-define(IFACE_MODE, alog_if).
-define(IFACE_SOURCE, "alog_if.erl").
-define(FILTER_BOOLEAN, ['>','<','=<','>=','==','/=']).

%%% API
%% @doc Makes alog_if_default parse transform
parse_transform(Forms, _Opt) ->
    make_default_ast(Forms).

%% @doc Loads new config to alog_if (logging module)
load_config(Config) ->
    case make_ast(Config) of
        {ok, NewAst} ->
            try load_config2(NewAst) of
                _ ->
                    ok
            catch
                Class:Exp ->
                    {error,{Class, Exp}}
            end;
        Other ->
            Other
    end.

%%% Internal functions
%% @private
load_config2(NewAst) ->
%    Source = erl_prettypr:format(erl_syntax:form_list(NewAst)),
%    io:format("~s~n",[Source]),
    {ok, ModuleName, Bin} = compile:forms(NewAst),
    code:load_binary(ModuleName, ?IFACE_SOURCE, Bin).

%% @private
make_ast(Config) ->
    case check_config(Config) of
        ok ->
            make_proceed_ast(Config);
        Other ->
            Other
    end.

%% @private
make_proceed_ast(Config) ->
    Clauses     = multiply_clauses(Config),
    DefAst      = alog_if_default:default_mod_ast(),
    NewAst      = insert_clauses(DefAst, Clauses),
    FlowSeq     = [X || #flow{id = X} <- Config],
    FlowNewAst  = insert_flowlist(NewAst, [abstract(FlowSeq)]), 
    {ok, FlowNewAst}.

%% @private
%% @doc makes many many clauses
multiply_clauses(Config) ->
    multiply_clauses(Config, def_clause()).
multiply_clauses([FlowRec|Configs], Acc) ->
    NewAcc = make_clause(FlowRec#flow.id,
			 FlowRec#flow.filter,
			 FlowRec#flow.priority,
			 FlowRec#flow.loggers,
			 FlowRec#flow.formatter,
			 Acc),
    multiply_clauses(Configs, NewAcc);
multiply_clauses([], Acc) ->
    Acc.

%% @private
make_clause(Flow, Filter, Prio, Loggers, Formatter, Acc) when is_tuple(Filter) ->
    make_clause(Flow, [Filter], Prio, Loggers, Formatter, Acc);    
make_clause(Flow, [F|Filters], Prio, Loggers, Formatter, Acc) ->
    Acc1 = make_clause_low(Flow, F, Prio, Loggers, Formatter, Acc),
    make_clause(Flow, Filters, Prio, Loggers, Formatter, Acc1);
make_clause(_,[], _, _,_, Acc) ->
    Acc.

%% @private
make_clause_low(Flow, {What, [ModTag|ModTags]}, Prio, Loggers, Formatter, Acc)  ->
    AbsLogs = [abstract({Formatter,Loggers})],
    NewClause = {clause, 0, get_arity(Flow,What,ModTag),
                 get_guard(Prio), AbsLogs},
    make_clause_low(Flow, {What, ModTags}, Prio, Loggers, Formatter,
		    [NewClause | Acc]);
make_clause_low(_,{_,[]}, _, _,_, Acc) ->
    Acc.

%% @private
%% @doc inserts all clauses to get_log_mods/3
insert_clauses([F|Fs], Clauses) ->
    F1 = insert_clauses_every(F, Clauses),
    Fs1 = insert_clauses(Fs, Clauses),
    [F1|Fs1];

insert_clauses([], _Ast) ->
    [].

%% @private
insert_clauses_every({function, Line, get_mod_logs,
                       Arity, _Clause}, Clauses) ->
    {function,Line,get_mod_logs,Arity,Clauses};
insert_clauses_every(Any, _Clauses) ->
    Any.

%% @private
insert_flowlist([F|Fs], FlowList) ->
    F1 = insert_flowlist_every(F, FlowList),
    Fs1 = insert_flowlist(Fs, FlowList), 
    [F1|Fs1];

insert_flowlist([], _FlowList) ->
    [].

%% @private
insert_flowlist_every({function, Line, flows, 
		       Arity, _Clause}, FlowList) ->
    NewClause = [{clause,0,[],[],FlowList}],
    {function, Line, flows, Arity, NewClause};
insert_flowlist_every(Any, _FlowList) ->
    Any.


%% @private
%% @doc checks config.
check_config([Rec|Configs]) ->
    case is_loaded(Rec#flow.priority, Rec#flow.loggers) of
	ok ->
	    check_config(Configs);
	Other ->
	    Other
    end;
check_config([]) ->
    ok.

%% @private
%% @doc Checks if modules are loaded
is_loaded(Prio, AllMods) ->
    Loaded = lists:filter(fun(Elem) ->
                                  case code:ensure_loaded(Elem) of
                                      {module, _} ->
                                          false;
                                      {error, _} ->
                                          true
                                  end
                          end, AllMods),
    case Loaded of
        [] ->
            check_prio(Prio) ;
        Other ->
            {error,{modules_not_loaded, Other}}
    end.

%% @private
%% @doc Checks guards
check_prio([P|All]) ->
    case check_prio(P) of
        ok ->
            check_prio(All);
        Other ->
            Other
    end;
check_prio([]) ->
    ok;
check_prio(Level) when is_integer(Level),
                       Level >= ?emergency, Level =< ?debug ->
    ok;
check_prio({Filter, Level}) when Level >= ?emergency, 
				 Level =< ?debug ->
    case lists:member(Filter,?FILTER_BOOLEAN) of
	true ->
	    ok;
	false ->
	    {error, {wrong_filter, Filter}}
    end;
check_prio(Other) ->
    {error, {wrong_level, Other}}.

%% @private
%% @doc Composes new AST for get_mod_logs/3
get_arity(Flow,mod, '_') ->
    [{integer, 0, Flow},{var,0,'Level'},{var,0,'_'},{var,0,'_'}];
get_arity(Flow,mod,Mod) ->
    [{integer, 0, Flow},{var,0,'Level'},{atom,0,Mod},{var,0,'_'}];
get_arity(Flow,tag,'_') ->
    [{integer, 0, Flow},{var,0,'Level'},{var,0,'_'},{var,0,'_'}];
get_arity(Flow,tag,Tag) ->
    [{integer, 0, Flow},{var,0,'Level'},{var,0,'_'},{atom,0,Tag}].

%% @private
get_guard(Prio) when is_list(Prio) ->
    get_guard(Prio, []);
get_guard(Prio) when is_integer(Prio);is_tuple(Prio) ->
    [get_guard_low(Prio)].
get_guard([G|Gs], Acc) ->
    get_guard(Gs, [get_guard_low(G)|Acc]);
get_guard([], Acc) -> Acc.

%% @private
get_guard_low({G, Level}) ->
    [{op,0,G,{var,0,'Level'},{integer,0, Level}}];
get_guard_low(Pri) when is_integer(Pri) ->
    [{op,0,'=:=',{var,0,'Level'},{integer,0,Pri}}].

%% @private
def_clause() ->
    {function,_Line,get_mod_logs,_Arity,DefCl} =
        alog_if_default:default_modlogs_ast(),
    DefCl.

%% @private
%% @doc Works during first compilation. AST of alog_if is written to
%% alog_is_default:default_mod_ast/0
make_default_ast(Forms) ->
    ModAst = abstract(change_and_remove(Forms)),
    Glm = abstract(find_gml(Forms)),
    mdma_transform(Forms, ModAst, Glm).

%% @private
find_gml([{function,Line,get_mod_logs,Arity,Clause}|_]) ->
    {function,Line,get_mod_logs,Arity,Clause};
find_gml([_|Fs]) ->
    find_gml(Fs);
find_gml([]) ->
    erlang:error("default interface is damaged").

%% @private
mdma_transform([F|Fs], Ast, Glm) ->
    F1 = mdma_transform_every(F, Ast, Glm),
    Fs1 = mdma_transform(Fs, Ast, Glm),
    [F1|Fs1];
mdma_transform([], _Ast, _Glm) -> [].

%% @private
mdma_transform_every({function,Line,default_mod_ast,Arity,Clause}, Ast, _Glm) ->
    NewClause = transform_clause(Clause, Ast),
    {function,Line,default_mod_ast,Arity,NewClause};

%% @private
mdma_transform_every({function,Line,default_modlogs_ast,Arity,Clause}, _Ast, Glm) ->
    NewClause = transform_clause(Clause, Glm),
    {function,Line,default_modlogs_ast,Arity,NewClause};

%% @private
mdma_transform_every(Node, _Ast, _Glm) ->
    Node.

%% @private
%% @doc Removes default_modlogs_ast/0 and default_mod_ast/0 from alog_if
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

%% @private
cmd_every({attribute,_,file,{_,_}}) ->
    {attribute,0,file,{?IFACE_SOURCE,0}};
cmd_every({attribute,_,module,_}) ->
    {attribute,1,module,?IFACE_MODE};
cmd_every({attribute,_, export, Funcs}) ->
    F1 = proplists:delete(default_mod_ast, Funcs),
    F2 = proplists:delete(default_modlogs_ast, F1),
    {attribute,1, export, F2};
cmd_every(Node) ->
    Node.

%% @private
transform_clause([{clause, Line, Op, Guard, _Result}|_],Ast) ->
    [{clause, Line, Op, Guard, [Ast]}].

%% @private
abstract(Term) ->
    erl_syntax:revert(erl_syntax:abstract(Term)).

