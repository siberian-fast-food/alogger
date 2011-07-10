Module alog_control
===================


<h1>Module alog_control</h1>

* [Function Index](#index)
* [Function Details](#functions)






__Behaviours:__ [`gen_server`](gen_server.md).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_new_flow-3">add_new_flow/3</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#delete_flow-1">delete_flow/1</a></td><td></td></tr><tr><td valign="top"><a href="#disable_flow-1">disable_flow/1</a></td><td></td></tr><tr><td valign="top"><a href="#dump_to_config-0">dump_to_config/0</a></td><td></td></tr><tr><td valign="top"><a href="#enable_flow-1">enable_flow/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_flows-0">get_flows/0</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#set_flow_priority-2">set_flow_priority/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#update_flow-2">update_flow/2</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="add_new_flow-3"></a>

<h3>add_new_flow/3</h3>





`add_new_flow(Filter, Priority, Loggers) -> any()`

<a name="code_change-3"></a>

<h3>code_change/3</h3>





`code_change(OldVsn, State, Extra) -> any()`

<a name="delete_flow-1"></a>

<h3>delete_flow/1</h3>





`delete_flow(Id) -> any()`

<a name="disable_flow-1"></a>

<h3>disable_flow/1</h3>





`disable_flow(Id) -> any()`

<a name="dump_to_config-0"></a>

<h3>dump_to_config/0</h3>





`dump_to_config() -> any()`

<a name="enable_flow-1"></a>

<h3>enable_flow/1</h3>





`enable_flow(Id) -> any()`

<a name="get_flows-0"></a>

<h3>get_flows/0</h3>





`get_flows() -> any()`

<a name="handle_call-3"></a>

<h3>handle_call/3</h3>





`handle_call(Request, From, State) -> any()`

<a name="handle_cast-2"></a>

<h3>handle_cast/2</h3>





`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

<h3>handle_info/2</h3>





`handle_info(Info, State) -> any()`

<a name="init-1"></a>

<h3>init/1</h3>





`init(X1) -> any()`

<a name="set_flow_priority-2"></a>

<h3>set_flow_priority/2</h3>





`set_flow_priority(Id, Priority) -> any()`

<a name="start_link-0"></a>

<h3>start_link/0</h3>





`start_link() -> any()`

<a name="terminate-2"></a>

<h3>terminate/2</h3>





`terminate(Reason, State) -> any()`

<a name="update_flow-2"></a>

<h3>update_flow/2</h3>





`update_flow(Id, Flow) -> any()`

