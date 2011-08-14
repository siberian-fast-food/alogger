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

-export([power_on/0, power_off/0, power_inverse/0]).

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
         replace_flows/1,
         dump_to_config/1,
         add_logger/1,
         delete_logger/1,
         replace_loggers/1,
         get_loggers/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%% Default enabled loggers
%% will be used in case if alog.config doesn't exist
-define(DEF_LOGGERS_ENABLED, [alog_tty]).
%% Default enabled flows
%% will be used in case if alog.config doesn't exist
-define(DEF_FLOWS_ENABLED, [{{mod, ['_']}, {'=<', debug}, [alog_tty]}]).

-type filter() :: {mod, atom()} | {mod, [atom()]} |
                  {tag, atom()} | {tag, [atom()]} |
                  {app, atom()}.

-type logger() :: atom().

-type priority() :: debug | info | notice | warning | error 
                    | critical | alert | emergency | integer().

-type priority_expr() :: '<' | '>' | '=<' | '>=' | '==' | '/='.

-type priority_pattern() :: list({priority_expr(), priority()}) |
                            {priority_expr(), priority()} | priority().

-record(flow, {id              :: non_neg_integer(),
               filter          :: filter(),
               priority        :: priority_pattern(),
               loggers  = []   :: list(logger()),
               enabled  = true :: true | {false, user} | {false, loggersOff}}).

-record(config, {flows           = [] :: list(#flow{}),
                 enabled_loggers = [] :: list(logger()),
                 power           = on :: on | off}).

power_on() ->
    gen_server:call(?SERVER, {set_power, on}).

power_off() ->
    gen_server:call(?SERVER, {set_power, off}).

power_inverse() ->
    gen_server:call(?SERVER, power_inverse).

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
-spec  set_flow_loggers(non_neg_integer(), list(logger())) ->
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
-spec  add_new_flow(filter(), priority_pattern(), [logger()])
                   -> ok | {error, term()}.
add_new_flow(Filter, Priority, Loggers) ->
    gen_server:call(?SERVER, {add_new_flow, Filter, Priority, Loggers}).

%% @doc Replace all flows on new.
-spec  replace_flows([#flow{}]) -> ok | {error, term()}.
replace_flows(Flows) ->
    gen_server:call(?SERVER, {replace_flows, Flows}).

%% @doc Update flows configuration in .config file
-spec dump_to_config(string()) -> ok | {error, term()}.
dump_to_config(File) ->
    gen_server:call(?SERVER, {dump_to_config, File}).

%% @doc Add and start new logger.
-spec add_logger(logger()) -> ok | {error, term()}.                       
add_logger(Logger) ->
    gen_server:call(?SERVER, {add_logger, Logger}).

%% @doc Delete exist logger. All flows use this logger
%% became disabled.
-spec delete_logger(logger()) -> ok | {error, term()}.                       
delete_logger(Logger) ->
    gen_server:call(?SERVER, {delete_logger, Logger}).

-spec replace_loggers([logger()]) -> ok |  {error, term()}.
replace_loggers(Loggers) ->
    gen_server:call(?SERVER, {replace_loggers, Loggers}).

-spec get_loggers() -> {ok, [logger()]}.
get_loggers() ->
    gen_server:call(?SERVER, get_loggers).

%% @private
init_loggers() ->
    gen_server:call(?SERVER, init_loggers).

%% @private
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%% gen_server callbacks
%% @private
init([]) ->
    EnabledLoggers = alog_config:get_conf(enabled_loggers,
                                          ?DEF_LOGGERS_ENABLED),
    OrdLoggers     = ordsets:from_list(EnabledLoggers),
    FlowsConfig    = alog_config:get_conf(flows,
                                          ?DEF_FLOWS_ENABLED),
    Flows          = add_flow(parse_flows_config(FlowsConfig), [], OrdLoggers),
    {ok, #config{enabled_loggers = OrdLoggers, flows = Flows}}.

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
    ok = stop_loggers(EnabledLoggers),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
%% @private
do_request(init_loggers, #config{enabled_loggers = EnabledLoggers} = Config) ->
    ok = start_loggers(EnabledLoggers),
    ok = apply_config(Config),

    case alog_config:get_conf(install_error_logger_handler, true) of
        true ->
            ok = alog_error_logger_handler:install();
        _ -> pass
    end,

    {ok, Config};

do_request(power_inverse, #config{power = Power} = Config) ->
    NewPower = case Power of
                   on  -> off;
                   off -> on
               end,
    NewConfig = Config#config{power = NewPower},
    new_config_if_successfully_applied(NewConfig, Config);
    
do_request({set_power, Val}, #config{} = Config) ->
    NewConfig = Config#config{power = Val},
    new_config_if_successfully_applied(NewConfig, Config);

do_request(get_flows, #config{flows = Flows} = Config) ->
    {{ok, Flows}, Config};

do_request(print_flows, #config{flows = Flows, power = Power} = Config) ->
    Header = case Power of
                 off -> {header, "ATTENTION: POWER OFF"};
                 on  -> {header, "POWER ON"}
             end,
    Table  = {table, {flows, list_to_tuple(record_info(fields, flow)),
                      [begin
                           [_ | NF] = tuple_to_list(F),
                           list_to_tuple(NF)
                       end || F <- Flows]}},
    format_lib_supp:print_info(group_leader(), [Header, Table]),
    {ok, Config};

do_request({add_new_flow, Filter, Priority, Loggers},
           #config{flows = Flows, enabled_loggers = EnabledLoggers} = Config) ->
    
    NewFlow = #flow{filter = Filter, priority = Priority,
                    loggers = Loggers},

    NewFlows = add_flow(NewFlow, Flows, EnabledLoggers),

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
    
    %%TODO check flow loggers
    ModFun =
        fun(Flow) ->
                {mod_flow, Flow#flow{loggers = ordsets:from_list(Loggers)}}
        end,
    modify_flow_if_exist(Id, Flows, ModFun, Config);

do_request({enable_flow, Id},
           #config{flows = Flows, enabled_loggers = EnabledLoggers}
           = Config) ->

    ModFun =
        fun(#flow{loggers = Loggers} = Flow) ->
                case ordsets:is_subset(Loggers, EnabledLoggers) of
                    true  -> {mod_flow, Flow#flow{enabled = true}};
                    false -> {mod_flow, Flow#flow{enabled = {false, loggersOff}}}
                end
           end,
    modify_flow_if_exist(Id, Flows, ModFun, Config);

do_request({disable_flow, Id},
           #config{flows = Flows} = Config) ->

    ModFun = fun(Flow) ->
                     {mod_flow, Flow#flow{enabled = {false, user}}}
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

do_request({replace_flows, NewFlows},
           #config{enabled_loggers = EnabledLoggers} =Config) ->
    CheckedFlows = add_flow(NewFlows, [], EnabledLoggers),
    NewConfig = Config#config{flows = CheckedFlows},
    new_config_if_successfully_applied(NewConfig, Config);

do_request({dump_to_config, File},#config{flows = Flows} = Config) ->
    {dump_to_config_low(File, Flows), Config};

do_request(get_loggers, #config{enabled_loggers = Loggers} = Config) ->
    {{ok, Loggers}, Config};

do_request({delete_logger, Logger}, #config{enabled_loggers = Loggers}
           = Config) ->
    replace_loggers(ordsets:del_element(Logger, Loggers), Loggers, Config);

do_request({add_logger, Logger}, #config{enabled_loggers = Loggers}
           = Config) ->
    replace_loggers(ordsets:add_element(Logger, Loggers), Loggers, Config);

do_request({replace_loggers, Loggers}, #config{enabled_loggers = Loggers}
           = Config) ->
    replace_loggers(ordsets:from_list(Loggers), Loggers, Config);

do_request(_Req, State) -> {{error, badreq}, State}.

add_flow(NewFlows, Flows, EnabledLoggers) when is_list(NewFlows)->
    MaxId =
        lists:foldl(fun(#flow{id = CurId}, MaxId) when CurId > MaxId -> CurId;
                       (_Flow, MaxId) -> MaxId
                    end, 0, Flows) + 1,

    CheckAndAddFlow =
        fun(#flow{loggers = Loggers, enabled = Enabled} = Flow,
            {CurId, FlowsAcc}) ->
                OrdLoggers = ordsets:from_list(Loggers),
                NewEnabled =
                    case ordsets:is_subset(OrdLoggers, EnabledLoggers) of
                        _ when Enabled =/= true -> {false, user};
                        true  -> true;
                        false -> {false, loggersOff}
                    end,
                {CurId +1, [Flow#flow{id = CurId,
                                      enabled = NewEnabled,
                                      loggers = OrdLoggers} | FlowsAcc]}
        end,
    {_NewMaxId, CheckedFlows} =
        lists:foldl(CheckAndAddFlow, {MaxId, Flows}, NewFlows),
    CheckedFlows;
add_flow(Flow, Flows, EnabledLoggers) ->
    add_flow([Flow], Flows, EnabledLoggers).

%% @private
replace_loggers(OldLoggers, OldLoggers, Config) ->
    {ok, Config};
replace_loggers(NewLoggers, OldLoggers, #config{flows = Flows} = Config) ->
    DeletedLoggers = ordsets:subtract(OldLoggers, NewLoggers),
    AddedLoggers   = ordsets:subtract(NewLoggers, OldLoggers),

    FlowSwitcher =
        fun(#flow{loggers = Loggers, enabled = true} = Flow) ->
                case ordsets:is_disjoint(Loggers, DeletedLoggers) of
                    true  -> Flow;
                    false -> Flow#flow{enabled = {false, loggersOff}}
                end;
           (#flow{enabled = {false, loggersOff},
                  loggers = Loggers} = Flow) ->
                case ordsets:is_subset(Loggers, NewLoggers) of
                    true  -> Flow#flow{enabled = true};
                    false -> Flow
                end;
           (UserDisabledFlow) -> UserDisabledFlow
        end,

    NewFlows  = lists:map(FlowSwitcher, Flows),
    NewConfig = Config#config{enabled_loggers = NewLoggers,
                              flows           = NewFlows},

    ok = start_loggers(AddedLoggers),

    case new_config_if_successfully_applied(NewConfig, Config) of
        {ok, _} ->
            ok = stop_loggers(DeletedLoggers),
            {ok, NewConfig};
        ErrorAndOldConfig ->
            ok = stop_loggers(AddedLoggers),
            ErrorAndOldConfig
    end.

start_loggers(Loggers) ->
    [begin
         LoggerConfig = alog_config:get_conf(Logger, []),
         ok = Logger:start([{sup_ref, alog_sup} | LoggerConfig])
     end || Logger <- Loggers],
    ok.

stop_loggers(Loggers) ->
    [begin
         LoggerConfig = alog_config:get_conf(Logger, []),
         ok = Logger:stop([{sup_ref, alog_sup} | LoggerConfig])
     end  || Logger <- Loggers],
    ok.

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
        {error,Error} -> {{error, Error}, OldConfig}
    catch
        E:W ->
            {{E, {W, erlang:get_stacktrace()}}, OldConfig}
    end.

%% @private
apply_config(#config{power = off}) ->
    alog_parse_trans:load_config([]);
apply_config(#config{flows = Flows}) ->
    alog_parse_trans:load_config(configs_to_internal_form(Flows)).

%% @private
configs_to_internal_form(Flows) ->
    ToInternaFlow =
        fun(#flow{enabled = true, filter = Filter, loggers = Loggers,
                  priority = PriorityPattern}, Acc) ->
                NewFlow =
                    {filter_to_internal(Filter),
                     priority_pattern_to_internal(PriorityPattern),
                     Loggers},
                [NewFlow | Acc];
           (_DisabledFlow, Acc) -> Acc
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
