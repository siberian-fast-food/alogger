-module(alog_config).

-export([
         get_conf/1,
         get_conf/2
        ]).

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

