-module(alog_control).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_flows/0,
         set_flow_priority/2,
         disable_flow/1,
         enable_flow/1,
         delete_flow/1,
         add_new_flow/3,
         update_flow/2,
         dump_to_config/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-type filter() :: {mod, atom()} | {mod, [atom()]} |
                  {tag, atom()} | {tag, [atom()]}.

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

set_flow_priority(Id, Priority) ->
    gen_server:call(?SERVER, {set_flow_priority, Id, Priority}).

disable_flow(Id) ->
    gen_server:call(?SERVER, {disable_flow, Id}).

enable_flow(Id) ->
    gen_server:call(?SERVER, {enable_flow, Id}).

delete_flow(Id) ->
    gen_server:call(?SERVER, {delete_flow, Id}).

add_new_flow(Filter, Priority, Loggers) ->
    gen_server:call(?SERVER, {add_new_flow, Filter, Priority, Loggers}).

update_flow(Id, Flow) ->
    gen_server:call(?SERVER, {update_flow, Id, Flow}).

dump_to_config() ->
    gen_server:call(?SERVER, dump_to_config).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% gen_server callbacks
init([]) ->
    EnabledLoggers = alog_config:get_conf(enabled_loggers, []),
    FlowsConfig      = alog_config:get_conf(flows, []),
    self() ! init_loggers,
    {ok, #config{enabled_loggers = EnabledLoggers,
                 flows = parse_flows_config(FlowsConfig)
                }}.

handle_call(Request, _From, State) ->
    {Reply, NewState} = do_request(Request, State),
    {reply, Reply, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init_loggers, #config{enabled_loggers = EnabledLoggers} = Config) ->
    [
     begin
         LoggerConfig = alog_config:get_conf(Logger, []),
         ok = Logger:start([{sup_ref, alog_sup} | LoggerConfig])
     end
     || Logger <- EnabledLoggers
    ],

    apply_config(Config),

    {noreply, Config};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
do_request(get_flows, #config{flows = Flows} = Config) ->
    {{ok, Flows}, Config};

do_request({add_new_flow, Filter, Priority, Loggers},
           #config{flows = Flows} = Config) ->

    Id = length(Flows) + 1,
    NewFlows = [#flow{id = Id, filter = Filter, priority = Priority,
                      loggers = Loggers} | Flows],

    {ok, Config#config{flows = NewFlows}};

do_request(_Req, State) -> {{error, badreq}, State}.

apply_config(#config{flows = Flows}) ->
    alog_parse_trans:load_config(configs_to_internal_form(Flows)).

configs_to_internal_form(Flows) ->
    ToInternaFlow = 
        fun(#flow{enabled = false}, Acc) -> Acc;
           (#flow{filter = Filter, loggers = Loggers,
                  priority = {P, Priority}}, Acc) ->
                NewFlow =
                    {filter_to_internal(Filter),
                     {P, priority_to_internal(Priority)},
                     Loggers},
                [NewFlow | Acc]
        end,
    lists:foldl(ToInternaFlow, [], Flows).

priority_to_internal(emergency)-> 0;
priority_to_internal(alert)->     1;
priority_to_internal(critical)->  2;
priority_to_internal(error)->     3;
priority_to_internal(warning)->   4;
priority_to_internal(notice)->    5;
priority_to_internal(info)->      6;
priority_to_internal(debug)->     7.

filter_to_internal(Filter) -> Filter.

parse_flows_config(FlowsConfig) ->
    ParseFun =
        fun({Filter, Priority, Loggers}, {CurId, Flows}) ->
                Flow = #flow{id       = CurId,
                             filter   = Filter,
                             priority = Priority,
                             loggers  = Loggers
                            },
                {CurId + 1, [Flow | Flows]}
        end,
    {_Id, ParsedFlows} = lists:foldl(ParseFun, {1, []}, FlowsConfig),
    ParsedFlows.
