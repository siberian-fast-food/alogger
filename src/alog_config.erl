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
%% Inner wrapper allows to abstract from a particular source of configuration.
%% @end
%% ----------------------------------------------------------------------

-module(alog_config).

-export([get_conf/1,
         get_conf/2]).

-define(app, alog).

%% @doc Returns the value of the configuration property Prop.
%% if configuration parameter does not exist, the function returns undefined
get_conf(Prop) -> get_conf(Prop, undefined).

%% @doc Returns the value of the configuration property Prop.
%% if configuration parameter does not exist, the function returns DefVal
get_conf(Prop, DefVal) ->
    Mod = get_config_module(),
    get_conf(Mod, Prop, DefVal).

%% @private
get_conf(application, Prop, DefVal) ->
    value_or_default(application:get_env(?app, Prop), DefVal);
get_conf(Mod, Prop, DefVal) ->
    value_or_default(Mod:get_conf(Prop), DefVal).

%% @private
get_config_module() ->
    ConfigModule = application:get_env(?app, config_module),
    value_or_default(ConfigModule, application).

%% @private
value_or_default({ok, Value}, _DefVal) -> Value;
value_or_default(_,           DefVal)  -> DefVal.
