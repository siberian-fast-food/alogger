Module alog_control
===================


<h1>Module alog_control</h1>

* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



Main interface for work with log flows.



__Behaviours:__ [`gen_server`](gen_server.md).


<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-filter">filter()</a></h3>




<pre>filter() = {mod, atom()} | {mod, [atom()]} | {tag, atom()} | {tag, [atom()]} | {app, atom()}</pre>



<h3 class="typedecl"><a name="type-priority">priority()</a></h3>




<pre>priority() = debug | info | notice | warning | error | critical | alert | emergency | integer()</pre>



<h3 class="typedecl"><a name="type-priority_expr">priority_expr()</a></h3>




<pre>priority_expr() = '<' | '>' | '=<' | '>=' | '==' | '/='</pre>



<h3 class="typedecl"><a name="type-priority_pattern">priority_pattern()</a></h3>




<pre>priority_pattern() = [{<a href="#type-priority_expr">priority_expr()</a>, <a href="#type-priority">priority()</a>}] | {<a href="#type-priority_expr">priority_expr()</a>, <a href="#type-priority">priority()</a>} | <a href="#type-priority">priority()</a></pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_new_flow-3">add_new_flow/3</a></td><td>Add new flow.</td></tr><tr><td valign="top"><a href="#delete_all_flows-0">delete_all_flows/0</a></td><td>Delete all flows.</td></tr><tr><td valign="top"><a href="#delete_flow-1">delete_flow/1</a></td><td>Delete existing flow.</td></tr><tr><td valign="top"><a href="#disable_flow-1">disable_flow/1</a></td><td>Temporary disable existing flow.</td></tr><tr><td valign="top"><a href="#dump_to_config-1">dump_to_config/1</a></td><td>Update flows configuration in .config file.</td></tr><tr><td valign="top"><a href="#enable_flow-1">enable_flow/1</a></td><td>Enable existing flow.</td></tr><tr><td valign="top"><a href="#get_flows-0">get_flows/0</a></td><td>Return all flows.</td></tr><tr><td valign="top"><a href="#print_flows-0">print_flows/0</a></td><td>Print all flows.</td></tr><tr><td valign="top"><a href="#replase_flows-1">replase_flows/1</a></td><td>Replase all flows on new.</td></tr><tr><td valign="top"><a href="#set_flow_filter-2">set_flow_filter/2</a></td><td>Set new filter for existing flow.</td></tr><tr><td valign="top"><a href="#set_flow_loggers-2">set_flow_loggers/2</a></td><td>Set new loggers for existing flow.</td></tr><tr><td valign="top"><a href="#set_flow_priority-2">set_flow_priority/2</a></td><td>Set new priority_pattern for existing flow.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="add_new_flow-3"></a>

<h3>add_new_flow/3</h3>





<pre>add_new_flow(Filter::<a href="#type-filter">filter()</a>, Priority::<a href="#type-priority_pattern">priority_pattern()</a>, Loggers::[atom()]) -> ok | {error, term()}</pre>
<br></br>




Add new flow.<a name="delete_all_flows-0"></a>

<h3>delete_all_flows/0</h3>





<pre>delete_all_flows() -> ok | {error, term()}</pre>
<br></br>




Delete all flows.<a name="delete_flow-1"></a>

<h3>delete_flow/1</h3>





<pre>delete_flow(Id::non_neg_integer()) -> ok | {error, term()}</pre>
<br></br>




Delete existing flow.<a name="disable_flow-1"></a>

<h3>disable_flow/1</h3>





<pre>disable_flow(Id::non_neg_integer()) -> ok | {error, term()}</pre>
<br></br>




Temporary disable existing flow.<a name="dump_to_config-1"></a>

<h3>dump_to_config/1</h3>





<pre>dump_to_config(File::string()) -> ok | {error, term()}</pre>
<br></br>




Update flows configuration in .config file<a name="enable_flow-1"></a>

<h3>enable_flow/1</h3>





<pre>enable_flow(Id::non_neg_integer()) -> ok | {error, term()}</pre>
<br></br>




Enable existing flow.<a name="get_flows-0"></a>

<h3>get_flows/0</h3>





<pre>get_flows() -> {ok, [#flow{}]} | {error, term()}</pre>
<br></br>




Return all flows.<a name="print_flows-0"></a>

<h3>print_flows/0</h3>





<pre>print_flows() -> ok</pre>
<br></br>




Print all flows.<a name="replase_flows-1"></a>

<h3>replase_flows/1</h3>





<pre>replase_flows(Flow::[#flow{}]) -> ok | {error, term()}</pre>
<br></br>




Replase all flows on new.<a name="set_flow_filter-2"></a>

<h3>set_flow_filter/2</h3>





<pre>set_flow_filter(Id::non_neg_integer(), Filter::<a href="#type-filter">filter()</a>) -> ok | {error, term()}</pre>
<br></br>




Set new filter for existing flow.<a name="set_flow_loggers-2"></a>

<h3>set_flow_loggers/2</h3>





<pre>set_flow_loggers(Id::non_neg_integer(), Loggers::[atom()]) -> ok | {error, term()}</pre>
<br></br>




Set new loggers for existing flow.<a name="set_flow_priority-2"></a>

<h3>set_flow_priority/2</h3>





<pre>set_flow_priority(Id::non_neg_integer(), Priority::<a href="#type-priority_pattern">priority_pattern()</a>) -> ok | {error, term()}</pre>
<br></br>




Set new priority_pattern for existing flow.