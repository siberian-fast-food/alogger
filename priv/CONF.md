Configuration
==============

alogger may be configured to write different flows to different loggers.
It is configured in alog.config.

        [{alog, [	
                  {enabled_loggers, Loggers},
                  {flows, [
                          {Filter,PriorityPattern, Loggers},
                  ]},
		  {LoggerName,ListOfSettings}
                ]}
         ].
	 



**enabled_loggers** - list of loggers, enabled by default.

**flows** - every flow is represented as tuple of setting.


`{Filter, PriorityPattern, Loggers}`

**Filter** = {tag, TagList} | {mod, ModList}

* TagList is list of tags, Every tag is atom. If you set tag as filter, printouts with pointed tags is sent to loggers. Name of modules are  no matter in this case.

* ModList is list of modules which should be logged to loggers. Tags are no matter.

**PriorityPattern** = [{Exp, PrioName}] | [PrioName] | {Exp, PrioName} | PrioName

* Exp -  arithmetic expression filter for priority. >=, =<, >, <, =:=, /= are possible. 

* PrioName - name of priority. Possible: emergency, critical, error, warning, notice, info, debug 

**Loggers** = [Logger]

* Logger is atom. Name of logger.



`{LoggerName, ListOfSettings}`


**LoggerName** - name of logger module.

**ListOfSettings** = [Setting]

* Setting is for logger.

	 
