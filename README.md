

<h1>The alog application</h1>

Overview
========


__Authors:__ Alexander Dergachev ([`alexander.dergachev@gmail.com`](mailto:alexander.dergachev@gmail.com)), Artem Golovinsky ([`artemgolovinsky@gmail.com`](mailto:artemgolovinsky@gmail.com)), Igor Karymov ([`ingham.k@gmail.com`](mailto:ingham.k@gmail.com)), Dmitry Groshev ([`lambdadmitry@gmail.com`](mailto:lambdadmitry@gmail.com)).
License
-------
<pre>
Copyright (c) 2011 Siberian Fast Food
Authors: Alexander Dergachev alexander.dergachev@gmail.com
         Artem Golovinsky    artemgolovinsky@gmail.com
         Igor Karymov        ingham.k@gmail.com
         Dmitry Groshev      lambdadmitry@gmail.com
The contents of this file are subject to the Erlang Public License,
Version 1.1, (the "License"); you may not use this file except in
compliance with the License. You should have received a copy of the
Erlang Public License along with this software. If not, it can be
retrieved online at http://www.erlang.org/.

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
the License for the specific language governing rights and limitations
under the License.
</pre>



Rationale
---------
There is a lot of loggers in the wild. You can consider to use one of them in your new project. After a while a new cool logger appears, and you want it, so, you have to change lots of places in you code, and definitely you have to recompile the entire project. That sucks, doesn't it?



The Abstract Logger Interface (alogger) brings completely new opportunities of logging. Here you go:


  * the possibility to introduce any new logger to your project without a need to change the project code at all

  * the interface will provide some neat features which are not exist in most of the existent loggers, like these:
    
    * the possibility to change the logging priority "on the fly" without any significant overhead thanks to some parse_transform and hot code reloading magic

    * the possibility to change the logging priority by modules or by special tags

    
 





So, basically, with alogger you'll be able to add any new logger to you project, you even won't have to recompile your code. You'll be able to control all your log flows
from the shell or through the config file.



We don't invent a wheel, there's nothing like this around!

Implemented logger interfaces
-----------------------------
A the moment there're four logger interfaces out of the box:

* __alog_tty__: a simple one, it prints logs by io:format

* __alog_syslog__: an interface towards Syslog daemon

* __alog_scribe__: an interface towards Scribe log daemon through thrift protocol

* __alog_disk_log__: an interface towards disk_log logging facility





How to use alogger and feel some magic
--------------------------------------
You can use alogger in different ways.

**Using macroses:**
you can use only .hrl file like this
<pre>-include_lib("alog.hrl")</pre>
and you will get standart ?DBG/?INFO/?ERROR/... (you can find out more information in [`alog`](https://github.com/siberian-fast-food/alogger/blob/master/doc/alog.md) module, which contains function that mimic macroses names and arguments). Or you can also engage our parse transformation:
<pre>
-include_lib("alog_pt.hrl").
</pre>
This way you can use _tuple expression_ (like ?DBG({A, B})) which are translated to debug message with both names and values of A and B. Tuple expression can contain strings (like ?DBG({A, "string", B})); a log message will contain something like <pre>alog_examples:42:debug [< 0.52.0 >]->[]: A: "foo" B: bar</pre>



**Using runtime API:**
this API consists of pure function calls. Functions are defined in [`alog`](https://github.com/siberian-fast-food/alogger/blob/master/doc/alog.md) module.

Configuration
-------------
alogger can be configured to write different flows (or streams of log messages) to different loggers. It can be done in config (alog.config) or in runtime with [`alog_control`](https://github.com/siberian-fast-food/alogger/blob/master/doc/alog_control.md) module.
<pre>
        [{alog, [
                  {enabled_loggers, Loggers},
                  {flows, [
                          {Filter,PriorityPattern, Loggers},
                  ]},
		  {LoggerName,ListOfSettings}
                ]}
         ].
</pre>



**enabled_loggers** - list of loggers, enabled by default.



**flows** - every flow is represented as tuple of setting.

<pre>{Filter, PriorityPattern, Loggers}</pre>



**Filter** = {tag, TagList} | {mod, ModList}


* TagList is list of tags, Every tag is atom. If you set tag as filter, printouts with pointed tags is sent to loggers. Name of modules are  no matter in this case.

* ModList is list of modules which should be logged to loggers. Tags are no matter. When ModList is set to ['\_'] log message from any module will be printed. Also you can set exceptions using [{'\_', not_for, ExModList}] where ExModList is list of modules which should not be logged.  





**PriorityPattern** = [{Exp, PrioName}] | [PrioName] | {Exp, PrioName} | PrioName


* Exp -  arithmetic expression filter for priority. >=, =<, >, <, =:=, /= are possible.

* PrioName - name of priority. Possible: emergency, critical, error, warning, notice, info, debug





**Loggers** = [Logger]


* Logger is atom. Name of logger.



<pre>{LoggerName, ListOfSettings}</pre>



**LoggerName** - name of logger module.



**ListOfSettings** = [Setting]


* Setting is for logger.





How to run examples
-------------------
We prepared some simple logging examples, so you could check how it works. You can run it by the following command from the Erlang shell:

<pre>
> alog_examples:run_examples().
</pre>



NOTE: if you have enabled alog_scribe logger interface, you should have Scribe log daemon installed and configured (an configuration example you can find in the priv directory). For more information about Scribe installation procedure see Scribe documentation.



How to implement your own interface
-----------------------------------
To implement your own interface you should introduce a module which implements the gen_alogger behaviour. It's possible to configure your interface modules through priv/alog.config file. For each log interface module there's a configuration entry, for example:

<pre>
{alog_syslog, [{ident, "alogger"},
               {logopt, [cons, perror, pid]},
               {facility, user}]}
</pre>



Also, gen_logger behaviour provides two helper functions: get_opt/2 and get_opt/3, you can use it in your start/1 function to get neccessary configuration, for example:

<pre>
start(Opts) ->
    Ident = gen_alogger:get_opt(ident, Opts, ?DEF_IDENT),
    Logopt = gen_alogger:get_opt(logopt, Opts, ?DEF_LOGOPT),
    Facility = gen_alogger:get_opt(facility, Opts, ?DEF_FACILITY),
...
</pre>

Log levels
----------
Log levels are arranged in the following order.


<table>
<tr><th>Level</th><th>Description</th></tr>
<tr><td>0. emergency</td><td>system is unusable</td></tr>
<tr><td>1. alert</td><td>action must be taken immediately</td></tr>
<tr><td>2. critical</td><td>critical conditions</td></tr>
<tr><td>3. error</td><td>error conditions</td></tr>
<tr><td>4. warning</td><td>warning conditions</td></tr>
<tr><td>5. notice</td><td>normal but significant condition</td></tr>
<tr><td>6. info</td><td>informational</td></tr>
<tr><td>7. debug</td><td>debug-level messages</td></tr>
</table>




For example, emergency < error, and debug > warning.

Last updated
------------
Aug 7 2011 23:56:44


<h2 class="indextitle">Packages</h2>



<table width="100%" border="0" summary="list of packages"><tr><td><a href="https://github.com/siberian-fast-food/alogger/blob/master/scribe/package-summary.md" class="package">scribe</a></td></tr></table>



<h2 class="indextitle">Modules</h2>



<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/siberian-fast-food/alogger/blob/master/doc/alog.md" class="module">alog</a></td></tr>
<tr><td><a href="https://github.com/siberian-fast-food/alogger/blob/master/doc/alog_common_formatter.md" class="module">alog_common_formatter</a></td></tr>
<tr><td><a href="https://github.com/siberian-fast-food/alogger/blob/master/doc/alog_config.md" class="module">alog_config</a></td></tr>
<tr><td><a href="https://github.com/siberian-fast-food/alogger/blob/master/doc/alog_control.md" class="module">alog_control</a></td></tr>
<tr><td><a href="https://github.com/siberian-fast-food/alogger/blob/master/doc/alog_disk_log.md" class="module">alog_disk_log</a></td></tr>
<tr><td><a href="https://github.com/siberian-fast-food/alogger/blob/master/doc/alog_error_logger_handler.md" class="module">alog_error_logger_handler</a></td></tr>
<tr><td><a href="https://github.com/siberian-fast-food/alogger/blob/master/doc/alog_examples.md" class="module">alog_examples</a></td></tr>
<tr><td><a href="https://github.com/siberian-fast-food/alogger/blob/master/doc/alog_if_default.md" class="module">alog_if_default</a></td></tr>
<tr><td><a href="https://github.com/siberian-fast-food/alogger/blob/master/doc/alog_parse_trans.md" class="module">alog_parse_trans</a></td></tr>
<tr><td><a href="https://github.com/siberian-fast-food/alogger/blob/master/doc/alog_pt.md" class="module">alog_pt</a></td></tr>
<tr><td><a href="https://github.com/siberian-fast-food/alogger/blob/master/doc/alog_scribe.md" class="module">alog_scribe</a></td></tr>
<tr><td><a href="https://github.com/siberian-fast-food/alogger/blob/master/doc/alog_syslog.md" class="module">alog_syslog</a></td></tr>
<tr><td><a href="https://github.com/siberian-fast-food/alogger/blob/master/doc/alog_tty.md" class="module">alog_tty</a></td></tr>
<tr><td><a href="https://github.com/siberian-fast-food/alogger/blob/master/doc/gen_alogger.md" class="module">gen_alogger</a></td></tr></table>

