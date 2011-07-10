Module alog
===========


<h1>Module alog</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)



This module is a main alog module.



__Behaviours:__ [`application`](application.md).

<h2><a name="description">Description</a></h2>

It serves start/0 and
stop/0 functions as a user API and implements appication behaviour.
It also contains runtime logging API that mimics macroses defined in
alog.hrl. There are 7 log levels (emergency, alert, critical, error,
warning, notice, info and debug) and 3 functions for each log
level - the one that accepts format string, arguments and list of tags,
the one without tags and the one with string only.

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#alert-1">alert/1</a></td><td></td></tr><tr><td valign="top"><a href="#alert-2">alert/2</a></td><td></td></tr><tr><td valign="top"><a href="#alert-3">alert/3</a></td><td></td></tr><tr><td valign="top"><a href="#critical-1">critical/1</a></td><td></td></tr><tr><td valign="top"><a href="#critical-2">critical/2</a></td><td></td></tr><tr><td valign="top"><a href="#critical-3">critical/3</a></td><td></td></tr><tr><td valign="top"><a href="#dbg-1">dbg/1</a></td><td></td></tr><tr><td valign="top"><a href="#dbg-2">dbg/2</a></td><td></td></tr><tr><td valign="top"><a href="#dbg-3">dbg/3</a></td><td></td></tr><tr><td valign="top"><a href="#emergency-1">emergency/1</a></td><td></td></tr><tr><td valign="top"><a href="#emergency-2">emergency/2</a></td><td></td></tr><tr><td valign="top"><a href="#emergency-3">emergency/3</a></td><td></td></tr><tr><td valign="top"><a href="#error-1">error/1</a></td><td></td></tr><tr><td valign="top"><a href="#error-2">error/2</a></td><td></td></tr><tr><td valign="top"><a href="#error-3">error/3</a></td><td></td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td></td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td></td></tr><tr><td valign="top"><a href="#info-3">info/3</a></td><td></td></tr><tr><td valign="top"><a href="#log-4">log/4</a></td><td></td></tr><tr><td valign="top"><a href="#notice-1">notice/1</a></td><td></td></tr><tr><td valign="top"><a href="#notice-2">notice/2</a></td><td></td></tr><tr><td valign="top"><a href="#notice-3">notice/3</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Starts alog application.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>Stops alog application.</td></tr><tr><td valign="top"><a href="#warning-1">warning/1</a></td><td></td></tr><tr><td valign="top"><a href="#warning-2">warning/2</a></td><td></td></tr><tr><td valign="top"><a href="#warning-3">warning/3</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="alert-1"></a>

<h3>alert/1</h3>





<pre>alert(Format::nonempty_string()) -> ok</pre>
<br></br>


<a name="alert-2"></a>

<h3>alert/2</h3>





<pre>alert(Format::nonempty_string(), Args::list()) -> ok</pre>
<br></br>


<a name="alert-3"></a>

<h3>alert/3</h3>





<pre>alert(Format::nonempty_string(), Args::list(), Tag::[atom()]) -> ok</pre>
<br></br>


<a name="critical-1"></a>

<h3>critical/1</h3>





<pre>critical(Format::nonempty_string()) -> ok</pre>
<br></br>


<a name="critical-2"></a>

<h3>critical/2</h3>





<pre>critical(Format::nonempty_string(), Args::list()) -> ok</pre>
<br></br>


<a name="critical-3"></a>

<h3>critical/3</h3>





<pre>critical(Format::nonempty_string(), Args::list(), Tag::[atom()]) -> ok</pre>
<br></br>


<a name="dbg-1"></a>

<h3>dbg/1</h3>





<pre>dbg(Format::nonempty_string()) -> ok</pre>
<br></br>


<a name="dbg-2"></a>

<h3>dbg/2</h3>





<pre>dbg(Format::nonempty_string(), Args::list()) -> ok</pre>
<br></br>


<a name="dbg-3"></a>

<h3>dbg/3</h3>





<pre>dbg(Format::nonempty_string(), Args::list(), Tag::[atom()]) -> ok</pre>
<br></br>


<a name="emergency-1"></a>

<h3>emergency/1</h3>





<pre>emergency(Format::nonempty_string()) -> ok</pre>
<br></br>


<a name="emergency-2"></a>

<h3>emergency/2</h3>





<pre>emergency(Format::nonempty_string(), Args::list()) -> ok</pre>
<br></br>


<a name="emergency-3"></a>

<h3>emergency/3</h3>





<pre>emergency(Format::nonempty_string(), Args::list(), Tag::[atom()]) -> ok</pre>
<br></br>


<a name="error-1"></a>

<h3>error/1</h3>





<pre>error(Format::nonempty_string()) -> ok</pre>
<br></br>


<a name="error-2"></a>

<h3>error/2</h3>





<pre>error(Format::nonempty_string(), Args::list()) -> ok</pre>
<br></br>


<a name="error-3"></a>

<h3>error/3</h3>





<pre>error(Format::nonempty_string(), Args::list(), Tag::[atom()]) -> ok</pre>
<br></br>


<a name="info-1"></a>

<h3>info/1</h3>





<pre>info(Format::nonempty_string()) -> ok</pre>
<br></br>


<a name="info-2"></a>

<h3>info/2</h3>





<pre>info(Format::nonempty_string(), Args::list()) -> ok</pre>
<br></br>


<a name="info-3"></a>

<h3>info/3</h3>





<pre>info(Format::nonempty_string(), Args::list(), Tag::[atom()]) -> ok</pre>
<br></br>


<a name="log-4"></a>

<h3>log/4</h3>





<pre>log(Format::nonempty_string(), Args::list(), Level::integer(), Tags::[atom()]) -> ok</pre>
<br></br>


<a name="notice-1"></a>

<h3>notice/1</h3>





<pre>notice(Format::nonempty_string()) -> ok</pre>
<br></br>


<a name="notice-2"></a>

<h3>notice/2</h3>





<pre>notice(Format::nonempty_string(), Args::list()) -> ok</pre>
<br></br>


<a name="notice-3"></a>

<h3>notice/3</h3>





<pre>notice(Format::nonempty_string(), Args::list(), Tag::[atom()]) -> ok</pre>
<br></br>


<a name="start-0"></a>

<h3>start/0</h3>





`start() -> any()`



Starts alog application<a name="stop-0"></a>

<h3>stop/0</h3>





`stop() -> any()`



Stops alog application<a name="warning-1"></a>

<h3>warning/1</h3>





<pre>warning(Format::nonempty_string()) -> ok</pre>
<br></br>


<a name="warning-2"></a>

<h3>warning/2</h3>





<pre>warning(Format::nonempty_string(), Args::list()) -> ok</pre>
<br></br>


<a name="warning-3"></a>

<h3>warning/3</h3>





<pre>warning(Format::nonempty_string(), Args::list(), Tag::[atom()]) -> ok</pre>
<br></br>


