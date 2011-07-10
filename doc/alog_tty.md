Module alog_tty
===============


<h1>Module alog_tty</h1>

* [Function Index](#index)
* [Function Details](#functions)






__Behaviours:__ [`gen_alogger`](gen_alogger.md).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-6">format/6</a></td><td>returns formated log message.</td></tr><tr><td valign="top"><a href="#log-2">log/2</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="format-6"></a>

<h3>format/6</h3>





<pre>format(FormatString::string(), Args::[term()], Tag::string(), Module::atom(), Line::integer(), Pid::pid()) -> <a href="#type-io_list">io_list()</a></pre>
<br></br>




returns formated log message<a name="log-2"></a>

<h3>log/2</h3>





<pre>log(ALoggerPrio::integer(), Msg::string()) -> ok</pre>
<br></br>


<a name="start-1"></a>

<h3>start/1</h3>





<pre>start(SupRef::term()) -> ok</pre>
<br></br>


<a name="stop-0"></a>

<h3>stop/0</h3>





<pre>stop() -> ok</pre>
<br></br>


