Module alog_syslog
==================


<h1>Module alog_syslog</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)



The log interface towards system Syslog daemon.



__Behaviours:__ [`gen_alogger`](gen_alogger.md).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-6">format/6</a></td><td>returns formated log message.</td></tr><tr><td valign="top"><a href="#log-2">log/2</a></td><td>logs message Msg with apropriate priority.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>starts syslog driver and opens log with predefined
configuration.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="format-6"></a>

<h3>format/6</h3>





<pre>format(FormatString::string(), Args::[term()], Tag::string(), Module::atom(), Line::integer(), Pid::pid()) -> iolist()</pre>
<br></br>




returns formated log message<a name="log-2"></a>

<h3>log/2</h3>





<pre>log(ALoggerPrio::integer(), Msg::string()) -> ok</pre>
<br></br>




logs message Msg with apropriate priority<a name="start-1"></a>

<h3>start/1</h3>





<pre>start(Opts::list()) -> ok</pre>
<br></br>




starts syslog driver and opens log with predefined
configuration<a name="stop-1"></a>

<h3>stop/1</h3>





<pre>stop(X1::list()) -> ok</pre>
<br></br>


