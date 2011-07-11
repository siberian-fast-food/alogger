Module alog_common_formatter
============================


<h1>Module alog_common_formatter</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)



The default log message formatter.



<h2><a name="description">Description</a></h2>

You can use it for formating
your log messages in your own log interfaces like we do in
alog_syslog, alog_tty and alog_scribe.

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-7">format/7</a></td><td>returns formated log message.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="format-7"></a>

<h3>format/7</h3>





`format(Msg, Args, Level, Tag, Module, Line, Pid) -> any()`



returns formated log message