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

-module(alog_config).

-export([get_conf/1,
         get_conf/2]).

-define(app, alog).

get_conf(Prop) -> get_conf(Prop, undefined).

get_conf(Prop, DefVal) ->
    Mod = get_config_module(),
    get_conf(Mod, Prop, DefVal).

get_conf(application, Prop, DefVal) ->
    value_or_default(application:get_env(?app, Prop), DefVal);
get_conf(Mod, Prop, DefVal) ->
    value_or_default(Mod:get_conf(Prop), DefVal).

get_config_module() ->
    ConfigModule = application:get_env(?app, config_module),
    value_or_default(ConfigModule, application).

value_or_default({ok, Value}, _DefVal) -> Value;
value_or_default(_,           DefVal)  -> DefVal.

