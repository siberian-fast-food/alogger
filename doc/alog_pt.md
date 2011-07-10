Module alog_pt
==============


<h1>Module alog_pt</h1>

* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



This module contains parse_transform to give user an ability to use
"tuple expressions": ?DBG({A, "string",  B}) will log string
"A: [A value] string B: [B value]".




<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-forms">forms()</a></h3>




<pre>forms() = [<a href="#type-stxtree">stxtree()</a>]</pre>



<h3 class="typedecl"><a name="type-options">options()</a></h3>




<pre>options() = [{atom(), any()}]</pre>



<h3 class="typedecl"><a name="type-stxtree">stxtree()</a></h3>




<pre>stxtree() = tuple()</pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse_transform-2">parse_transform/2</a></td><td>This API function makes actual parse transformation.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="parse_transform-2"></a>

<h3>parse_transform/2</h3>





<pre>parse_transform(Forms::<a href="#type-forms">forms()</a>, Opts::<a href="#type-options">options()</a>) -> <a href="#type-forms">forms()</a></pre>
<br></br>




This API function makes actual parse transformation