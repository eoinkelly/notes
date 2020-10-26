
Sources

* https://www.liquidweb.com/kb/series/apache-performance-tuning/

Apache has many ways of doing multiprocessing


1. MPM Prefork
    * parent process spawns worker children
    * each child has a single thread
    * required if you are running a module which isn't threadsfe e.g. mod_php
    * -- can be resource intensive comapred to other MPM options
1. MPM Worker
    * parent process spawns worker children
    * each child can have many threads
    * more efficient than MPM Prefork in most cases
1. MPM Event
    * Similar to MPM worker
    * Each child process has a listener thread which directs request to a worker thread
    * helps avoid threads getting stuck with long KeepAliveTimeouts
1. mpm_winnt
    * Windows MPM
1. Others
    * there are a few more niche MPM options

The exact configuration directives available depend on which MPM module you are using

Default apache `Timeout` is 60s which is quite long - maybe we should shorten?
    What is the timeout for an ELB?
