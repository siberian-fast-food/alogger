%% @private
%% @doc
%% This module containts tests for alogger. It uses eunit to make tests.
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

-module(alog_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("alog.hrl").

-define(all_priorities, [?emergency,
                         ?alert,
                         ?critical,
                         ?error,
                         ?warning,
                         ?notice,
                         ?info,
                         ?debug]).

-record(setup_state, {
          backup_flows
         }).

base_test_() ->
    {setup,
     fun install_test_logger_iface/0,
     fun remove_test_logger_iface/1,
     [{"all priority",
       ?_test(begin
                  ok = set_max_priority(),
                  [?assertEqual(ok, priority_work(P)) || P <- ?all_priorities]
              end)},
      {"priority barriers",
       ?_test([begin
                   ok = set_priority(CP),
                   GreatPriority = get_great_priorities(CP),
                   [?assertEqual(error, priority_work(P)) || P <- GreatPriority],
                   LowPriority = get_low_priorities(CP),
                   [?assertEqual(ok, priority_work(P)) || P <- [CP | LowPriority]]
               end || CP <- ?all_priorities])}]}.

install_test_logger_iface() ->
    ok = application:start(sasl),
    ok = alog:start(),
    {ok, BackupFlows} = alog_control:get_flows(),
    ok = alog_control:delete_all_flows(),
    MaxPr = get_max_priotity(),
    ok = alog_control:add_logger(alog_test_logger_iface),
    ok = alog_control:add_new_flow(
           [
            {filter,    {mod, [?MODULE]}},
            {priority,  {'=<', MaxPr}},
            {loggers,   [alog_test_logger_iface]},
            {formatter, alog_test_formatter},
            {enabled,   true}
           ]),
    #setup_state{backup_flows = BackupFlows}.

remove_test_logger_iface(#setup_state{backup_flows = BackupFlows}) ->
    ok = alog_control:replace_flows(BackupFlows),
    ok = alog_control:delete_logger(alog_test_logger_iface),
    alog:stop(),
    application:stop(sasl),
    ok.

get_great_priorities(P) -> [Rp || Rp <- ?all_priorities, Rp > P].

get_low_priorities(P) -> [Rp || Rp <- ?all_priorities, Rp < P].

get_max_priotity() -> lists:max(?all_priorities).

set_max_priority() -> set_priority(get_max_priotity()).

set_priority(P) ->
    ok = alog_control:set_flow_priority(1, {'=<', P}),
    ok.

priority_work(?debug)   ->
    Ref = make_ref(),
    ?DBG("test debug ~p", [Ref]),
    check_priority_work(?debug, Ref);
priority_work(?info)   ->
    Ref = make_ref(),
    ?INFO("test info ~p", [Ref]),
    check_priority_work(?info, Ref);
priority_work(?notice)   ->
    Ref = make_ref(),
    ?NOTICE("test notice ~p", [Ref]),
    check_priority_work(?notice, Ref);
priority_work(?warning)   ->
    Ref = make_ref(),
    ?WARNING("test warning ~p", [Ref]),
    check_priority_work(?warning, Ref);
priority_work(?error)   ->
    Ref = make_ref(),
    ?ERROR("test error ~p", [Ref]),
    check_priority_work(?error, Ref);
priority_work(?critical)   ->
    Ref = make_ref(),
    ?CRITICAL("test critical ~p", [Ref]),
    check_priority_work(?critical, Ref);
priority_work(?alert)   ->
    Ref = make_ref(),
    ?ALERT("test alert ~p", [Ref]),
    check_priority_work(?alert, Ref);
priority_work(?emergency)   ->
    Ref = make_ref(),
    ?EMERGENCY("test emergency ~p", [Ref]),
    check_priority_work(?emergency, Ref).

check_priority_work(Level, Ref) ->
    receive
        {format, Ref, Level, _Tag, _Module, _Line, _Pid} ->
	    receive
                {log, Level, Ref, _Tag2, _Module2, _Line2, _Pid2} -> ok;
                _ ->
		    error
            after 100 ->
                    error
            end;
         _ ->
            error
    after 100 ->
            error
    end.

-endif.
