Module alog_scribe
==================


<h1>Module alog_scribe</h1>

* [Function Index](#index)
* [Function Details](#functions)






__Behaviours:__ [`gen_alogger`](gen_alogger.md), [`gen_server`](gen_server.md).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#format-6">format/6</a></td><td>returns formated log message.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#log-2">log/2</a></td><td>logs message Msg with apropriate priority.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="code_change-3"></a>

<h3>code_change/3</h3>





`code_change(OldVsn, State, Extra) -> any()`

<a name="format-6"></a>

<h3>format/6</h3>





<pre>format(FormatString::string(), Args::[term()], Tag::string(), Module::atom(), Line::integer(), Pid::pid()) -> string()</pre>
<br></br>




returns formated log message<a name="handle_call-3"></a>

<h3>handle_call/3</h3>





`handle_call(Msg, From, State) -> any()`

<a name="handle_cast-2"></a>

<h3>handle_cast/2</h3>





`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

<h3>handle_info/2</h3>





`handle_info(Msg, StateData) -> any()`

<a name="init-1"></a>

<h3>init/1</h3>





`init(X1) -> any()`

<a name="log-2"></a>

<h3>log/2</h3>





<pre>log(ALoggerPrio::integer(), Msg::string()) -> ok</pre>
<br></br>




logs message Msg with apropriate priority<a name="start-1"></a>

<h3>start/1</h3>





<pre>start(Opts::list()) -> ok</pre>
<br></br>


<a name="start_link-0"></a>

<h3>start_link/0</h3>





<pre>start_link() -> pid()</pre>
<br></br>


<a name="stop-1"></a>

<h3>stop/1</h3>





<pre>stop(Opts::list()) -> ok</pre>
<br></br>


<a name="terminate-2"></a>

<h3>terminate/2</h3>





`terminate(Reason, State) -> any()`

