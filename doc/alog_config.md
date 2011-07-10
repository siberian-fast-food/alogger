Module alog_config
==================


<h1>Module alog_config</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)



Inner wrapper allows to abstract from a particular source of configuration.



<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_conf-1">get_conf/1</a></td><td>Returns the value of the configuration property Prop.</td></tr><tr><td valign="top"><a href="#get_conf-2">get_conf/2</a></td><td>Returns the value of the configuration property Prop.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="get_conf-1"></a>

<h3>get_conf/1</h3>





`get_conf(Prop) -> any()`



Returns the value of the configuration property Prop.
if configuration parameter does not exist, the function returns undefined<a name="get_conf-2"></a>

<h3>get_conf/2</h3>





`get_conf(Prop, DefVal) -> any()`



Returns the value of the configuration property Prop.
if configuration parameter does not exist, the function returns DefVal