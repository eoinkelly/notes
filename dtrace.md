# Dtrace

Sources

* http://www.oracle.com/technetwork/server-storage/solaris10/solaris-dtrace-wp-167895.pdf

* Official guide <http://dtrace.org/guide/preface.html>
* Book
    * <http://www.amazon.com/dp/0132091518>
    * Code <http://www.dtracebook.com/index.php/Main_Page>


* DTrace can instrument ALL software:
    * userland stuff: databases, apps etc.
    * kernel code
    * device drivers
* short for "dynamic tracking"
    * dynamic tracing is an instrumentation technique pioneered by DTrace
    * dynamically patches live running instructions with instrumentation code
* it can also do static tracing
    * user-friendly trace points are compiled-in that dtrace can hook into
* provides the `D` language for creating tracing scripts (looks like C and awk had a child)
    * D allows you to summarize data in kernel space before being passed over to userland which makes it fast enough to use on production systems.
* There are various GUIs that use DTrace underneath
    * Apple's Instruments
    * Joyent's Cloud Analytics
    * ZFS Storage Analytics

* The DTrace Toolkit
    * MacOS includes a bunch of scripts from this toolkit in `/usr/bin`

> Mac OS X has a flag (P_LNOATTACH) that a program may set that disallows tracing of that process by debugging utilities such as DTrace and gdb

* Language providers
    * allows DTRace to see more context of the code
    * support for Ruby, Erlang
* Application providers
    * Allows DTrace to follow execution from app to kernel and back
    * Postgres and MySQL support this
* Linux support
    * Oracle announced official implementation in 2012 but not released yet
    * A linux port has existed since 2008
    * `SystemTap` does a similar thing on linux
    * Linux seems to achieve similar goals with a mix of tools
        * http://www.brendangregg.com/linuxperf.html (NOTE: this page has lots of GREAT resources for profiling system performance on Linux)
