

<h1>The alog application</h1>

Overview
========


__Authors:__ Alexander Dergachev ([`alexander.dergachev@gmail.com`](mailto:alexander.dergachev@gmail.com)), Artem Golovinsky ([`artemgolovinsky@gmail.com`](mailto:artemgolovinsky@gmail.com)), Igor Karymov ([`ingham.k@gmail.com`](mailto:ingham.k@gmail.com)), Dmitry Groshev ([`lambdadmitry@gmail.com`](mailto:lambdadmitry@gmail.com)).

?? module.



Configuration
-------------

alogger may be configured to write different flows (or streams of log messages) to different loggers.
It is configured in alog.config.
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

* ModList is list of modules which should be logged to loggers. Tags are no matter.





**PriorityPattern** = [{Exp, PrioName}] | [PrioName] | {Exp, PrioName} | PrioName


* Exp -  arithmetic expression filter for priority. >=, =<, >, <, =:=, /= are possible.

* PrioName - name of priority. Possible: emergency, critical, error, warning, notice, info, debug





**Loggers** = [Logger]


* Logger is atom. Name of logger.



<pre>{LoggerName, ListOfSettings}</pre>



**LoggerName** - name of logger module.



**ListOfSettings** = [Setting]


* Setting is for logger.



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
Jul 11 2011 02:33:23


<h2 class="indextitle">Packages</h2>



<table width="100%" border="0" summary="list of packages"><tr><td><a href="https://github.com/spawnfest/alogger/blob/master/scribe/package-summary.md" class="package">scribe</a></td></tr></table>



<h2 class="indextitle">Modules</h2>



<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/spawnfest/alogger/blob/master/doc/alog.md" class="module">alog</a></td></tr>
<tr><td><a href="https://github.com/spawnfest/alogger/blob/master/doc/alog_common_formatter.md" class="module">alog_common_formatter</a></td></tr>
<tr><td><a href="https://github.com/spawnfest/alogger/blob/master/doc/alog_config.md" class="module">alog_config</a></td></tr>
<tr><td><a href="https://github.com/spawnfest/alogger/blob/master/doc/alog_control.md" class="module">alog_control</a></td></tr>
<tr><td><a href="https://github.com/spawnfest/alogger/blob/master/doc/alog_error_logger_handler.md" class="module">alog_error_logger_handler</a></td></tr>
<tr><td><a href="https://github.com/spawnfest/alogger/blob/master/doc/alog_examples.md" class="module">alog_examples</a></td></tr>
<tr><td><a href="https://github.com/spawnfest/alogger/blob/master/doc/alog_if_default.md" class="module">alog_if_default</a></td></tr>
<tr><td><a href="https://github.com/spawnfest/alogger/blob/master/doc/alog_parse_trans.md" class="module">alog_parse_trans</a></td></tr>
<tr><td><a href="https://github.com/spawnfest/alogger/blob/master/doc/alog_pt.md" class="module">alog_pt</a></td></tr>
<tr><td><a href="https://github.com/spawnfest/alogger/blob/master/doc/alog_scribe.md" class="module">alog_scribe</a></td></tr>
<tr><td><a href="https://github.com/spawnfest/alogger/blob/master/doc/alog_syslog.md" class="module">alog_syslog</a></td></tr>
<tr><td><a href="https://github.com/spawnfest/alogger/blob/master/doc/alog_tty.md" class="module">alog_tty</a></td></tr>
<tr><td><a href="https://github.com/spawnfest/alogger/blob/master/doc/gen_alogger.md" class="module">gen_alogger</a></td></tr></table>

