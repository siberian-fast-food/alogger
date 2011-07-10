%% @doc
%% Main interface for work with log flows.
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

-module(alog_control).
-behaviour(gen_server).
-include_lib("alog.hrl").

%% API
-export([start_link/0,
         init_loggers/0]).

-export([print_flows/0,
         get_flows/0,
         set_flow_priority/2,
         set_flow_filter/2,
         set_flow_loggers/2,
         disable_flow/1,
         enable_flow/1,
         delete_flow/1,
         delete_all_flows/0,
         add_new_flow/3,
         replase_flows/1,
         dump_to_config/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-type filter() :: {mod, atom()} | {mod, [atom()]} |
                  {tag, atom()} | {tag, [atom()]} |
                  {app, atom()}.

-type priority() :: debug | info | notice | warning | error 
                    | critical | alert | emergency | integer().

-type priority_expr() :: '<' | '>' | '=<' | '>=' | '==' | '/='.

-type priority_pattern() :: list({priority_expr(), priority()}) |
                            {priority_expr(), priority()} | priority().

-record(flow, {id              :: non_neg_integer(),
               filter          :: filter(),
               priority        :: priority_pattern(),
               loggers  = []   :: list(atom()),
               enabled  = true :: boolean()}).

-record(config, {flows           = [] :: list(#flow{}),
                 enabled_loggers = [] :: list(atom())}).


%% @doc Return all flows.
-spec get_flows() -> {ok, [#flow{}]} | {error, term()}.
get_flows() ->
    gen_server:call(?SERVER, get_flows).

%% @doc Print all flows.
-spec print_flows() -> ok.
print_flows() ->
    gen_server:call(?SERVER, print_flows).

%% @doc Set new priority_pattern for existing flow.
-spec  set_flow_priority(non_neg_integer(), priority_pattern()) ->
                                ok | {error, term()}.
set_flow_priority(Id, Priority) ->
    gen_server:call(?SERVER, {set_flow_priority, Id, Priority}).

%% @doc Set new filter for existing flow.
-spec  set_flow_filter(non_neg_integer(), filter()) ->
                                ok | {error, term()}.
set_flow_filter(Id, Filter) ->
    gen_server:call(?SERVER, {set_flow_filter, Id, Filter}).

%% @doc Set new loggers for existing flow.
-spec  set_flow_loggers(non_neg_integer(), list(atom())) ->
                                ok | {error, term()}.
set_flow_loggers(Id, Loggers) ->
    gen_server:call(?SERVER, {set_flow_loggers, Id, Loggers}).

%% @doc Temporary disable existing flow.
-spec  disable_flow(non_neg_integer()) -> ok | {error, term()}.
disable_flow(Id) ->
    gen_server:call(?SERVER, {disable_flow, Id}).

%% @doc Enable existing flow.
-spec  enable_flow(non_neg_integer()) -> ok | {error, term()}.
enable_flow(Id) ->
    gen_server:call(?SERVER, {enable_flow, Id}).

%% @doc Delete existing flow.
-spec  delete_flow(non_neg_integer()) -> ok | {error, term()}.
delete_flow(Id) ->
    gen_server:call(?SERVER, {delete_flow, Id}).

%% @doc Delete all flows.
-spec  delete_all_flows() -> ok | {error, term()}.
delete_all_flows() ->
    gen_server:call(?SERVER, delete_all_flows).

%% @doc Add new flow.
-spec  add_new_flow(filter(), priority_pattern(), [atom()])
                   -> ok | {error, term()}.
add_new_flow(Filter, Priority, Loggers) ->
    gen_server:call(?SERVER, {add_new_flow, Filter, Priority, Loggers}).

%% @doc Replase all flows on new.
-spec  replase_flows([#flow{}]) -> ok | {error, term()}.
replase_flows(Flows) ->
    gen_server:call(?SERVER, {replase_flows, Flows}).

%% @doc Update flows configuration in .config file
-spec dump_to_config(string()) -> ok | {error, term()}.
dump_to_config(File) ->
    gen_server:call(?SERVER, {dump_to_config, File}).

%% @private
init_loggers() ->
    gen_server:call(?SERVER, init_loggers).

%% @private
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%% gen_server callbacks
%% @private
init([]) ->
    EnabledLoggers = alog_config:get_conf(enabled_loggers, []),
    FlowsConfig    = alog_config:get_conf(flows, []),
    {ok, #config{enabled_loggers = EnabledLoggers,
                 flows = parse_flows_config(FlowsConfig)}}.

%% @private
handle_call(Request, _From, State) ->
    {Reply, NewState} = do_request(Request, State),
    {reply, Reply, NewState}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #config{enabled_loggers = EnabledLoggers} = Config) ->
    apply_config(Config#config{flows = []}),

    [begin
         LoggerConfig = alog_config:get_conf(Logger, []),
         ok = Logger:stop([{sup_ref, alog_sup} | LoggerConfig])
     end  || Logger <- EnabledLoggers],

    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
%% @private
do_request(init_loggers, #config{enabled_loggers = EnabledLoggers} = Config) ->
    [begin
         LoggerConfig = alog_config:get_conf(Logger, []),
         ok = Logger:start([{sup_ref, alog_sup} | LoggerConfig])
     end || Logger <- EnabledLoggers],

    apply_config(Config),

    case alog_config:get_conf(install_error_logger_handler, true) of
        true ->
            ok = alog_error_logger_handler:install();
        _ -> pass
    end,

    {ok, Config};

do_request(get_flows, #config{flows = Flows} = Config) ->
    {{ok, Flows}, Config};

do_request(print_flows, #config{flows = Flows} = Config) ->
    FormatFun = fun(#flow{id = Id, filter = Filter,
                          loggers = Loggers, enabled = Enabled
                         }, {FormatString, Vars}) ->
                        F = "id = ~w filter = ~w loggers = ~w enabled = ~w~n",
                        {FormatString ++ F,
                         Vars ++ [Id, Filter, Loggers, Enabled]}
                end,

    {Format, Args} = lists:foldl(FormatFun, {"",[]}, Flows),

    io:format(Format, Args),

    {ok, Config};

do_request({add_new_flow, Filter, Priority, Loggers},
           #config{flows = Flows} = Config) ->

    Id = length(Flows) + 1,
    NewFlows = [#flow{id = Id, filter = Filter, priority = Priority,
                      loggers = Loggers} | Flows],
    NewConfig = Config#config{flows = NewFlows},
    new_config_if_successfully_applied(NewConfig, Config);

do_request({set_flow_priority, Id, Priority},
           #config{flows = Flows} = Config) ->

    ModFun = fun(Flow) ->
                     {mod_flow, Flow#flow{priority = Priority}}
             end,
    modify_flow_if_exist(Id, Flows, ModFun, Config);

do_request({set_flow_filter, Id, Filter},
           #config{flows = Flows} = Config) ->

    ModFun = fun(Flow) ->
                     {mod_flow, Flow#flow{filter = Filter}}
             end,
    modify_flow_if_exist(Id, Flows, ModFun, Config);

do_request({set_flow_loggers, Id, Loggers},
           #config{flows = Flows} = Config) ->

    ModFun = fun(Flow) ->
                     {mod_flow, Flow#flow{loggers = Loggers}}
             end,
    modify_flow_if_exist(Id, Flows, ModFun, Config);

do_request({enable_flow, Id},
           #config{flows = Flows} = Config) ->

    ModFun = fun(Flow) ->
                     {mod_flow, Flow#flow{enabled = true}}
             end,
    modify_flow_if_exist(Id, Flows, ModFun, Config);

do_request({disable_flow, Id},
           #config{flows = Flows} = Config) ->

    ModFun = fun(Flow) ->
                     {mod_flow, Flow#flow{enabled = false}}
             end,
    modify_flow_if_exist(Id, Flows, ModFun, Config);

do_request({delete_flow, Id},
           #config{flows = Flows} = Config) ->

    ModFun = fun(_Flow) ->
                     {mod_flows, lists:keydelete(Id, #flow.id, Flows)}
             end,
    modify_flow_if_exist(Id, Flows, ModFun, Config);

do_request(delete_all_flows, Config) ->
    NewConfig = Config#config{flows = []},
    new_config_if_successfully_applied(NewConfig, Config);

do_request({replase_flows, NewFlows}, Config) ->
    NewConfig = Config#config{flows = NewFlows},
    new_config_if_successfully_applied(NewConfig, Config);

do_request({dump_to_config, File},#config{flows = Flows} = Config) ->
    {dump_to_config_low(File, Flows), Config};

do_request(_Req, State) -> {{error, badreq}, State}.

%% @private
modify_flow_if_exist(Id, Flows, ModFun, Config) ->
    case lists:keyfind(Id, #flow.id, Flows) of
        false ->
            {{error, {undefined_flow, Id}}, Config};
        Flow ->
            case ModFun(Flow) of
                {mod_flow, ModFlow} ->
                    NewFlows = lists:keyreplace(Id, #flow.id, Flows, ModFlow),
                    NewConfig = Config#config{flows = NewFlows},
                    new_config_if_successfully_applied(NewConfig, Config);
                {mod_flows, ModFlows} ->
                    NewConfig = Config#config{flows = ModFlows},
                    new_config_if_successfully_applied(NewConfig, Config);
                {config, ModConfig}->
                    new_config_if_successfully_applied(ModConfig, Config);
                Error ->
                    {{error, Error}, Config}
            end
    end.

%% @private
new_config_if_successfully_applied(NewConfig, OldConfig) ->
    try apply_config(NewConfig) of
        ok    -> {ok, NewConfig};
        Error -> {{error, Error}, OldConfig}
    catch
        E:W ->
            {{E, {W, erlang:get_stacktrace()}}, OldConfig}
    end.

%% @private
apply_config(#config{flows = Flows}) ->
    alog_parse_trans:load_config(configs_to_internal_form(Flows)).

%% @private
configs_to_internal_form(Flows) ->
    ToInternaFlow =
        fun(#flow{enabled = false}, Acc) -> Acc;
           (#flow{filter = Filter, loggers = Loggers,
                  priority = PriorityPattern}, Acc) ->
                NewFlow =
                    {filter_to_internal(Filter),
                     priority_pattern_to_internal(PriorityPattern),
                     Loggers},
                [NewFlow | Acc]
        end,
    lists:foldl(ToInternaFlow, [], Flows).

%% @private
priority_pattern_to_internal(PriorityPattern) when is_list(PriorityPattern) ->
    [priority_pattern_to_internal(Pp) || Pp <- PriorityPattern];
priority_pattern_to_internal({Exp, Priority}) ->
    {Exp, priority_to_internal(Priority)};
priority_pattern_to_internal(Priority) ->
    priority_to_internal(Priority).

%% @private
priority_to_internal(emergency) -> ?emergency;
priority_to_internal(alert)     -> ?alert;
priority_to_internal(critical)  -> ?critical;
priority_to_internal(error)     -> ?error;
priority_to_internal(warning)   -> ?warning;
priority_to_internal(notice)    -> ?notice;
priority_to_internal(info)      -> ?info;
priority_to_internal(debug)     -> ?debug;
priority_to_internal(P) when is_integer(P) -> P.

%% @private
filter_to_internal({app, AppName}) ->
    Modules =
        case application:get_all_key(AppName) of
            {ok, Params} ->
                proplists:get_value(modules, Params, []);
            _ -> []
        end,
    {mod, Modules};
filter_to_internal(Filter) -> Filter.

%% @private
parse_flows_config(FlowsConfig) ->
    ParseFun =
        fun({Filter, Priority, Loggers}, {CurId, Flows}) ->
                Flow = #flow{id       = CurId,
                             filter   = Filter,
                             priority = Priority,
                             loggers  = Loggers},
                {CurId + 1, [Flow | Flows]}
        end,
    {_Id, ParsedFlows} = lists:foldl(ParseFun, {1, []}, FlowsConfig),
    ParsedFlows.

%% @private
dump_to_config_low(File, Flow) ->
    case file:consult(File) of
	{ok, Terms} ->
	    update_terms(Terms, File, Flow);
	Error ->
	    Error
    end.

%% @private
update_terms(Terms, File, Flow) ->
    [[{alog, ListOfConf}]] = Terms,
    WriteToConf = [{alog, new_flow(ListOfConf, Flow)}], 
    write_to_config(WriteToConf, File).

%% @private
new_flow(ListOfConf, Flow) ->
    new_flow(ListOfConf, [], Flow).
new_flow([{flows, _}|Other], Acc, Flow) ->
    new_flow(Other, [{flows,compose_new_flow(Flow)}|Acc], Flow);
new_flow([N|Other],Acc, Flow) ->
    new_flow(Other, [N|Acc], Flow);
new_flow([], Acc, _Flow) ->
    lists:reverse(Acc).

%% @private
compose_new_flow(Flow) ->
    remove_id(Flow, []).

%% @private
remove_id([{flow,_, Filter, Exp, Loggers,_}|Other], Acc) ->
    remove_id(Other, [{Filter, Exp, Loggers}|Acc]);
remove_id([N|Oth], Acc) ->
    remove_id(Oth, [N|Acc]);
remove_id([], Acc) ->
    lists:reverse(Acc).

%% @private
write_to_config(Terms, File) ->
    file:write_file(File,io_lib:fwrite("~p.\n",[Terms])).
