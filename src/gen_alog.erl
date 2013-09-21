%% ----------------------------------------------------------------------
%% Copyright 2011-2013 alogger project
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc
%% Behaviour module for logger modules.
%% @end
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
     {format, 8},
     {reload, 1}];
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
