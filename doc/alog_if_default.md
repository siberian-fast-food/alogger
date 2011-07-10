Module alog_if_default
======================


<h1>Module alog_if_default</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)



This module is a blank for constructing parse_transformed module that
makes actual logging.



<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default_mod_ast-0">default_mod_ast/0</a></td><td>Will return default AST of this module after parse_transform.</td></tr><tr><td valign="top"><a href="#default_modlogs_ast-0">default_modlogs_ast/0</a></td><td>Will return default AST of get_logs_mod/3.</td></tr><tr><td valign="top"><a href="#log-7">log/7</a></td><td>Main logging function.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="default_mod_ast-0"></a>

<h3>default_mod_ast/0</h3>





`default_mod_ast() -> any()`



Will return default AST of this module after parse_transform<a name="default_modlogs_ast-0"></a>

<h3>default_modlogs_ast/0</h3>





`default_modlogs_ast() -> any()`



Will return default AST of get_logs_mod/3<a name="log-7"></a>

<h3>log/7</h3>





`log(Format, Args, Level, Tags, Module, Line, Pid) -> any()`



Main logging function