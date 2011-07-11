Module alog_syslog
==================


<h1>Module alog_syslog</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)



The log interface towards system Syslog daemon.



__Behaviours:__ [`gen_alogger`](gen_alogger.md).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-7">format/7</a></td><td>returns formated log message.</td></tr><tr><td valign="top"><a href="#log-2">log/2</a></td><td>logs message Msg with apropriate priority.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>starts syslog driver and opens log with predefined
configuration.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="format-7"></a>

<h3>format/7</h3>





`format(FormatString, Args, Level, Tag, Module, Line, Pid) -> any()`



returns formated log message<a name="log-2"></a>

<h3>log/2</h3>





`log(ALoggerPrio, Msg) -> any()`



logs message Msg with apropriate priority<a name="start-1"></a>

<h3>start/1</h3>





`start(Opts) -> any()`



starts syslog driver and opens log with predefined
configuration<a name="stop-1"></a>

<h3>stop/1</h3>





`stop(X1) -> any()`

