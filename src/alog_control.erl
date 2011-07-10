%% @doc
%% FIXME
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

-type priority() :: list() | tuple() | integer().

-record(flow, {id              :: non_neg_integer(),
               filter          :: filter(),
               priority        :: priority(),
               loggers  = []   :: list(atom()),
               enabled  = true :: boolean()}).

-record(config, {flows           = [] :: list(#flow{}),
                 enabled_loggers = [] :: list(atom())}).


%%% API
get_flows() ->
    gen_server:call(?SERVER, get_flows).

print_flows() ->
    gen_server:call(?SERVER, print_flows).

set_flow_priority(Id, Priority) ->
    gen_server:call(?SERVER, {set_flow_priority, Id, Priority}).

set_flow_filter(Id, Filter) ->
    gen_server:call(?SERVER, {set_flow_filter, Id, Filter}).

set_flow_loggers(Id, Loggers) ->
    gen_server:call(?SERVER, {set_flow_loggers, Id, Loggers}).

disable_flow(Id) ->
    gen_server:call(?SERVER, {disable_flow, Id}).

enable_flow(Id) ->
    gen_server:call(?SERVER, {enable_flow, Id}).

delete_flow(Id) ->
    gen_server:call(?SERVER, {delete_flow, Id}).

delete_all_flows() ->
    gen_server:call(?SERVER, delete_all_flows).

add_new_flow(Filter, Priority, Loggers) ->
    gen_server:call(?SERVER, {add_new_flow, Filter, Priority, Loggers}).

replase_flows(Flows) ->
    gen_server:call(?SERVER, {replase_flows, Flows}).

dump_to_config(File) ->
    gen_server:call(?SERVER, {dump_to_config, File}).

init_loggers() ->
    gen_server:call(?SERVER, init_loggers).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%%% gen_server callbacks
init([]) ->
    EnabledLoggers = alog_config:get_conf(enabled_loggers, []),
    FlowsConfig    = alog_config:get_conf(flows, []),
    {ok, #config{enabled_loggers = EnabledLoggers,
                 flows = parse_flows_config(FlowsConfig)}}.

handle_call(Request, _From, State) ->
    {Reply, NewState} = do_request(Request, State),
    {reply, Reply, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #config{enabled_loggers = EnabledLoggers} = Config) ->
    apply_config(Config#config{flows = []}),

    [begin
         LoggerConfig = alog_config:get_conf(Logger, []),
         ok = Logger:stop([{sup_ref, alog_sup} | LoggerConfig])
     end  || Logger <- EnabledLoggers],

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

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

new_config_if_successfully_applied(NewConfig, OldConfig) ->
    try apply_config(NewConfig) of
        ok    -> {ok, NewConfig};
        Error -> {{error, Error}, OldConfig}
    catch
        E:W ->
            {{E, {W, erlang:get_stacktrace()}}, OldConfig}
    end.

apply_config(#config{flows = Flows}) ->
    alog_parse_trans:load_config(configs_to_internal_form(Flows)).

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

priority_pattern_to_internal(PriorityPattern) when is_list(PriorityPattern) ->
    [priority_pattern_to_internal(Pp) || Pp <- PriorityPattern];
priority_pattern_to_internal({Exp, Priority}) ->
    {Exp, priority_to_internal(Priority)};
priority_pattern_to_internal(Priority) ->
    priority_to_internal(Priority).

priority_to_internal(emergency) -> ?emergency;
priority_to_internal(alert)     -> ?alert;
priority_to_internal(critical)  -> ?critical;
priority_to_internal(error)     -> ?error;
priority_to_internal(warning)   -> ?warning;
priority_to_internal(notice)    -> ?notice;
priority_to_internal(info)      -> ?info;
priority_to_internal(debug)     -> ?debug;
priority_to_internal(P) when is_integer(P) -> P.



filter_to_internal({app, AppName}) ->
    Modules =
        case application:get_all_key(AppName) of
            {ok, Params} ->
                proplists:get_value(modules, Params, []);
            _ -> []
        end,
    {mod, Modules};
filter_to_internal(Filter) -> Filter.

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

dump_to_config_low(File, Flow) ->
    case file:consult(File) of
	{ok, Terms} ->
	    update_terms(Terms, File, Flow);
	Error ->
	    Error
    end.

update_terms(Terms, File, Flow) ->
    [[{alog, ListOfConf}]] = Terms,
    WriteToConf = [{alog, new_flow(ListOfConf, Flow)}], 
    write_to_config(WriteToConf, File).

new_flow(ListOfConf, Flow) ->
    new_flow(ListOfConf, [], Flow).
new_flow([{flows, _}|Other], Acc, Flow) ->
    new_flow(Other, [{flows,compose_new_flow(Flow)}|Acc], Flow);
new_flow([N|Other],Acc, Flow) ->
    new_flow(Other, [N|Acc], Flow);
new_flow([], Acc, _Flow) ->
    lists:reverse(Acc).

compose_new_flow(Flow) ->
    remove_id(Flow, []).

remove_id([{flow,_, Filter, Exp, Loggers,_}|Other], Acc) ->
    remove_id(Other, [{Filter, Exp, Loggers}|Acc]);
remove_id([N|Oth], Acc) ->
    remove_id(Oth, [N|Acc]);
remove_id([], Acc) ->
    lists:reverse(Acc).

write_to_config(Terms, File) ->
    file:write_file(File,io_lib:fwrite("~p.\n",[Terms])).
