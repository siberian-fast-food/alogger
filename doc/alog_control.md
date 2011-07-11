Module alog_control
===================


<h1>Module alog_control</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)



Main interface for work with log flows.



__Behaviours:__ [`gen_server`](gen_server.md).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_new_flow-3">add_new_flow/3</a></td><td>Add new flow.</td></tr><tr><td valign="top"><a href="#delete_all_flows-0">delete_all_flows/0</a></td><td>Delete all flows.</td></tr><tr><td valign="top"><a href="#delete_flow-1">delete_flow/1</a></td><td>Delete existing flow.</td></tr><tr><td valign="top"><a href="#disable_flow-1">disable_flow/1</a></td><td>Temporary disable existing flow.</td></tr><tr><td valign="top"><a href="#dump_to_config-1">dump_to_config/1</a></td><td>Update flows configuration in .config file.</td></tr><tr><td valign="top"><a href="#enable_flow-1">enable_flow/1</a></td><td>Enable existing flow.</td></tr><tr><td valign="top"><a href="#get_flows-0">get_flows/0</a></td><td>Return all flows.</td></tr><tr><td valign="top"><a href="#print_flows-0">print_flows/0</a></td><td>Print all flows.</td></tr><tr><td valign="top"><a href="#replase_flows-1">replase_flows/1</a></td><td>Replase all flows on new.</td></tr><tr><td valign="top"><a href="#set_flow_filter-2">set_flow_filter/2</a></td><td>Set new filter for existing flow.</td></tr><tr><td valign="top"><a href="#set_flow_loggers-2">set_flow_loggers/2</a></td><td>Set new loggers for existing flow.</td></tr><tr><td valign="top"><a href="#set_flow_priority-2">set_flow_priority/2</a></td><td>Set new priority_pattern for existing flow.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="add_new_flow-3"></a>

<h3>add_new_flow/3</h3>





`add_new_flow(Filter, Priority, Loggers) -> any()`



Add new flow.<a name="delete_all_flows-0"></a>

<h3>delete_all_flows/0</h3>





`delete_all_flows() -> any()`



Delete all flows.<a name="delete_flow-1"></a>

<h3>delete_flow/1</h3>





`delete_flow(Id) -> any()`



Delete existing flow.<a name="disable_flow-1"></a>

<h3>disable_flow/1</h3>





`disable_flow(Id) -> any()`



Temporary disable existing flow.<a name="dump_to_config-1"></a>

<h3>dump_to_config/1</h3>





`dump_to_config(File) -> any()`



Update flows configuration in .config file<a name="enable_flow-1"></a>

<h3>enable_flow/1</h3>





`enable_flow(Id) -> any()`



Enable existing flow.<a name="get_flows-0"></a>

<h3>get_flows/0</h3>





`get_flows() -> any()`



Return all flows.<a name="print_flows-0"></a>

<h3>print_flows/0</h3>





`print_flows() -> any()`



Print all flows.<a name="replase_flows-1"></a>

<h3>replase_flows/1</h3>





`replase_flows(Flows) -> any()`



Replase all flows on new.<a name="set_flow_filter-2"></a>

<h3>set_flow_filter/2</h3>





`set_flow_filter(Id, Filter) -> any()`



Set new filter for existing flow.<a name="set_flow_loggers-2"></a>

<h3>set_flow_loggers/2</h3>





`set_flow_loggers(Id, Loggers) -> any()`



Set new loggers for existing flow.<a name="set_flow_priority-2"></a>

<h3>set_flow_priority/2</h3>





`set_flow_priority(Id, Priority) -> any()`



Set new priority_pattern for existing flow.