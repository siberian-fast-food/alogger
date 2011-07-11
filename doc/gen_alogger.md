Module gen_alogger
==================


<h1>Module gen_alogger</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)



Behaviour module for logger modules.



<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#behaviour_info-1">behaviour_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_opt-2">get_opt/2</a></td><td>returns the Opt-named option from the Opts proplist
or throws an exception in case of unsuccessful
lookup.</td></tr><tr><td valign="top"><a href="#get_opt-3">get_opt/3</a></td><td>returns the Opt-named option from the Opts proplist
or Default value in case of unsuccessful lookup.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="behaviour_info-1"></a>

<h3>behaviour_info/1</h3>





`behaviour_info(X1) -> any()`

<a name="get_opt-2"></a>

<h3>get_opt/2</h3>





`get_opt(Opt, Opts) -> any()`



returns the Opt-named option from the Opts proplist
or throws an exception in case of unsuccessful
lookup<a name="get_opt-3"></a>

<h3>get_opt/3</h3>





`get_opt(Opt, Opts, Default) -> any()`



returns the Opt-named option from the Opts proplist
or Default value in case of unsuccessful lookup