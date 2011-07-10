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

-record(flow, {id       :: non_neg_integer(),
               filter   :: filter(),
               priority :: priority(),
               loggers  :: list(atom())}).

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
    {ok, #config{}}.

handle_call(Request, _From, State) ->
    {Reply, NewState} = do_request(Request, State),
    {reply, Reply, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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
