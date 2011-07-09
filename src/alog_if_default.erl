-module(alog_if_default).

-compile([{parse_transform, alog_parse_trans}, nowarn_unused_vars]).

-export([get_logs_mod/3, log/7, default_mod_ast/0, default_logsmod_ast/0]).

% will return list of loggers for module/tag
get_logs_mod(_, _, _) ->
    [].

% main logging function
log(Level, Tags, Module, Line, Pid, Format, Args) when is_list(Tags) ->
    [log(Level, Tag, Module, Line, Pid, Format, Args) || Tag <- Tags];

% main logging function
log(Level, Tag, Module, Line, Pid, Format, Args) ->
        [begin                          
         Formatted = Mod:format(Format, Args, Tag, Module, Line, Pid),
         Mod:log(Level, Formatted)   
	 end || Mod <- get_logs_mod(Level, Module, Tag)].

% wille return default AST of this module after parse_transform
default_mod_ast() ->
    ok.

% will return default AST of get_logs_mod/3
default_logsmod_ast() ->
    ok.
