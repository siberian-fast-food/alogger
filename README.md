

<h1>The alog application</h1>

Overview
========


__Authors:__ Alexander Dergachev ([`alexander.dergachev@gmail.com`](mailto:alexander.dergachev@gmail.com)), Artem Golovinsky ([`artemgolovinsky@gmail.com`](mailto:artemgolovinsky@gmail.com)), Igor Karymov ([`ingham.k@gmail.com`](mailto:ingham.k@gmail.com)), Dmitry Groshev ([`lambdadmitry@gmail.com`](mailto:lambdadmitry@gmail.com)).
License
-------
<pre>
Copyright (c) 2011 Siberian Fast Food
Authors: Alexander Dergachev <alexander.dergachev@gmail.com>
         Artem Golovinsky    <artemgolovinsky@gmail.com>
         Igor Karymov        <ingham.k@gmail.com>
         Dmitry Groshev      <lambdadmitry@gmail.com>
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
Jul 11 2011 01:27:12


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

