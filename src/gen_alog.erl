%% @doc
%% Behaviour module for logger modules.
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

-module(gen_alog).
-author('alexander.dergachev@gmail.com').

-export([behaviour_info/1,
         get_opt/2,
         get_opt/3,
         set_opt/3]).

-spec behaviour_info(atom()) -> [{atom(), integer()}, ...] |
                                undefined.
behaviour_info(callbacks) ->
    [{start, 2},
     {stop, 2},
     {log, 3},
     {format, 8}];
behaviour_info(_) ->
    undefined.

%% @doc returns the Opt-named option from the Opts proplist
%%      or throws an exception in case of unsuccessful
%%      lookup
get_opt(Opt, Opts) ->
    case lists:keysearch(Opt, 1, Opts) of
      false ->
            % TODO: replace with more appropriate function
            throw({undefined_option, Opt});
      {value, {_, Val}} ->
        Val
    end.

%% @doc returns the Opt-named option from the Opts proplist
%%      or Default value in case of unsuccessful lookup
get_opt(Opt, Opts, Default) ->
    case lists:keysearch(Opt, 1, Opts) of
        false ->
            Default;
        {value, {_, Val}} ->
            Val
    end.

%% @doc sets the Opt-named option in the Opts proplist to Value
%%      and returns new options proplist
set_opt(Opt, Opts, Value) ->
    lists:keystore(Opt, 1, Opts, {Opt, Value}).
