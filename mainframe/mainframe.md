# Mainframes

- [Mainframes](#mainframes)
    - [Sources](#sources)
    - [Open questions](#open-questions)
    - [Overview](#overview)
    - [History](#history)
    - [Jargon](#jargon)
        - [CICS](#cics)
        - [DB2](#db2)
        - [OPC](#opc)
        - [VSAM](#vsam)
        - [Endevor ](#endevor)
        - [COBOL](#cobol)
        - [JCL](#jcl)
        - [Multiple Virtual Storage (MVS)](#multiple-virtual-storage-mvs)
        - [z/OS](#zos)
        - [Zowe](#zowe)
        - [Zowe Explorer](#zowe-explorer)
        - [HLASM](#hlasm)
        - [Code4z](#code4z)
        - [REXX (Restructured Extended Executor)](#rexx-restructured-extended-executor)
        - [RACF (Rack-eff Resource Access Control Facility)](#racf-rack-eff-resource-access-control-facility)
    - [The filesystem](#the-filesystem)
    - [Getting access to a mainframe environment to learn](#getting-access-to-a-mainframe-environment-to-learn)
        - [IBM z/OS Dev and Test (ZDT)](#ibm-zos-dev-and-test-zdt)
    - [z/OS Virtual Dev and Test (ZVDT)](#zos-virtual-dev-and-test-zvdt)
        - [IBM CLoud Wazi as a service](#ibm-cloud-wazi-as-a-service)
        - [Zexplore](#zexplore)
        - [Hercules + TK4-](#hercules--tk4-)
    - [Connecting to a mainframe](#connecting-to-a-mainframe)
        - [c3270 Tips](#c3270-tips)
        - [TSO tips](#tso-tips)
    - [Many OSes](#many-oses)
    - [CPUs vs other CPUS](#cpus-vs-other-cpus)
    - [The case against mainframes](#the-case-against-mainframes)
    - [Migrating from mainframe to cloud](#migrating-from-mainframe-to-cloud)

## Sources

- https://medium.com/modern-mainframe/beginners-guide-cobol-made-easy-introduction-ecf2f611ac76
- https://www.youtube.com/watch?v=YA3FQOzr0ag is really good - shows how to edit
  and compile a cobol program
    - shows how to use TOS and the mainframe editor
- https://www.ibm.com/docs/en/zos-basic-skills

## Open questions

- Can TOS side of mainframe do TCP? or is it only the unix side?
- Does anybody buy a mainframe in 2023 who didn't already own one and have a
  bunch of legacy jobs to keep running?
    - Does anybody buy a mainframe to just run the unix side? e.g a bunch of
      java stuff

## Overview

https://www.youtube.com/watch?v=NMRhPv1J08A

- The `Z` is short for `zero downtime`
- Almost always IBM Z hardware
- Usually means z/OS
    - IBM does support other OSes for Z hardware
    - z/OS cares a _lot_ about backwards compatibility
    - but it is an actively maintained and updated platform
- Built to handle huge amounts of I/O
- Built for Reliability, Availability, Serviceability) (RAS)
    - built for huge 9's uptime
- Built for backwards compatibility
- Batch oriented
- Mission-critical business systems
- Aside: AS/400 (more correctly called the "IBM i platform) is not a mainframe
    - it's the next tier down
- Can run modern stuff: SQL databases, Java
    - I think this runs in the unix subsystem
- Can also run batch jobs, COBOL from the '60's because of the backwards
  compatibility
- Huge machines can have 170 processors of different types:
    - CPUs can be dedicated to different jobs e.g. you have CPUs just for Java,
      others just for databases
    - 320GB up to TB in RAM
- Mainframes don't use ASCII for text encoding, they use EBCDIC
- LPARs = Logical partitions
    - multiple OSes/instances can run simultaneously on one mainframe
    - think dual booting but you run all the OSes simultaneously, don't think
      VMs
- Sysplex
    - Cluster of LPARs
    - Could be one mainframe, could be spread across multiple mainframes
      connected by fibre
- System programmer = sysprog = mainframe equivalent of a sysadmin
- IPL = Initial Program Load aka booting the mainframe
- Storage
    - Can refer to disks or memory
    - DASD Direct Access Storage Device) = hard drives
    - ALso used to virtual memory
- Almost everything on the mainframe is a job, managed by the Job Entry
  Subsystem (JES)
- MVS is a synonym for z/OS (it's the name for older versions for the mainframe
  OS)
- Interact with mainframe with TN3270 which actually was terminal hardware
    - modified version of telnet
    - in EBCDIC and color
    - Can be over SSL on port 992
- VTAM
    - The mainframe "portal"
    - ?? is this equivalent to a pre-OS env like BIOS on a PC?
    - Has splash screen usually
- TSO
    - the mainframe shell
- ISPF Interactive System Productivity Facility
    - mainframe GUI, very ncurses like
- USS/OMVS (Unix Sub-System/Open MVS)
    - Unix sub-system inside z/OS provides all the TCP/IP including HTTP and FTP
    - every mainframe includes a unix
    - was a cheap/practical way to get TCP/IP
    - is not linux, is an old and clunky unix
    - has an AIX flavour
- mainframe culture
    - very customisable
    - every install will be different
    - every company's mainframe env and jargon will be different
    - it's a huge box but it can be carved up into hundreds of OSes
- Security
    - not built-in to z/OS
    - most popular
        - RACF
            - by far the most popular
            - has a DB which olds everything for security (permissions, hashes,
              keys etc.)
            - `RVARY` command tells you the DB's location
        - CA ACF2
        - CA Top Secret
    - no real "root" concept but having SPECIAL attribute lets you grant
      yourself any access rights
    - historically very weak password rules and hash functions
    - this has gotten better but lots of sites have not upgraded
- Programming languages
    - Assembler (big-endian 64-bit z/Architecture)
    - COBOL
    - Fortran
    - PL/I
    - Java, C, C++ (all via USS, the unix subsystem)
- Scripting languages
    - JCL
    - CLIST (a list of TSO commands, bit like a shell script, TSO is quite
      limited)
    - REXX (closer to perl/python, more powerful than CLIST, most common thing
      for sysprogs to do their work in)
    - Normal shell script, perl etc. in USS
    - There is a python port from Rocket Software
- z/OS has a web server
    - will convert from EBCDIC to ASCII automatically
    - it also has WebSphere will it calls an "Application server"
    - web server can pass control to WebSphere or to a CGI script
- TSO commands

    ```sh
    # TSO commands
    LISTUSER
    LU # same as above

    PROFILE
    # your profile is akin to your .bashrc

    PING 1.2.3.4

    NETSTAT HOME

    LISTCAT # list all files

    # show info about the MYUSER.ABC file (the system will automatically add the prefix)
    LISTDS ABC

    # show members if it has them
    LISTDS ABC MEMBERS

    DEL FOO.HTML
    RENAME A.TXT B.TXT

    # no cat in tso but we can somewhat mimic with edit
    edit 'MYUSER.A.TXT'
    CNTL # tell it the filetype
    LIST
    END NOSAVE


    EXEC MYUSER.MYPROG # exec a program

    # PA1 acts like Ctrl-c to kill current process

    # touch
    # but you have to tell it how big the file will be etc. so this is the easiest way to make it like an existing file
    # allocating files in TSO is like allocating strings in C - you have to tell it all details
    ALLOCATE DATASET(TEST.DATA) LIKE(TEST.JCL)

    LOGOFF
    ```

- Most sysprogs don't use TSO directly, most use ISPF
- ISPF Interactive System Productivity Facility
    - number menu driven UI
    - Run `tso omvs` to open a unix promopt
    - `cp "//'MYMVSUSER.HELLO.TXT'" hello.txt` copy a file from the MVS
      filesystem to the unix filesystem
    - Run `tsocmd LISTC` you can run TSO commands with `tsocmd`
- TSO will tell you if you try to login twice (not great for security)
- FTP on z/OS can run uploaded JCL and SQL
    - RCE as a service
- Tools: TShOcker and a metasploit module
- Being root on the USS side does not give you SPECIAL or OPERATIONS on the MVS
  side

## History

- System/360
    - Announced 1964
- System/370
    - https://en.wikipedia.org/wiki/IBM_System/370
    - A range of 32 bit computers and also (confusingly) the name of the ISA
      they ran
    - Announced in 1970
    - Strong backwards compatibility story with System/360
- System/390
    - A range of computers and also (confusingly) the name of the ISA they ran
    - Announced in 1990, replacing System/370
    - ISA was ESA/390, the fifth generation of the System/360 instruction set
      architecture.
- z series
    - first computer was the z900
    - Announced in 2000
    - A 64bit CISC architecture
    - > z/Architecture retains backward compatibility with previous
      > 32-bit-data/31-bit-addressing architecture ESA/390 and its predecessors
      > all the way back to the 32-bit-data/24-bit-addressing System/360.

```
From https://en.wikipedia.org/wiki/IBM_System/360

History of IBM mainframes, 1952-present

Market names
    700/7000 series
    1400 series
    System/360
    System/370
    30XX series (303X, 308X, 3090)
    System/390
    eServer zSeries (900, 800; 990, 890)
    System z9
    System z10
    zEnterprise System (z196, zEC12)
    IBM Z (z13, z14, z15, z16)

Architectures
    System/360
    System/370
    S/370-XA
    ESA/370
    ESA/390
    z/Architecture
```

## Jargon

### CICS

- Customer information & control System (CICS) and Information Management System
  (IMS)
- CICS is much more popular than IMS
- Both CICS (pron. kicks) and IMS are _transaction managers_ - purpose built
  applications which allow normies to use the mainframe without having to learn
  TSO and ISPF
- These apps run over TN3270 but often have a web or desktop frontend
- Most of the security is client-side (and sucks) - The TN3270 protocol has
  quirks where you can say which fields are editable etc.
- More details in https://www.youtube.com/watch?v=3HFiv7NvWrM

### DB2

- SQL database from IBM

### OPC

> IBM Operations Planning and Control/ESA (OPC/ESA), a SystemView client/server
> product, is IBM's licensed program for managing systems production workload.
> OPC/ESA contributes to automating batch production workload

### VSAM

https://www.hcltech.com/blogs/modernizing-legacy-file-system-vsam-rdbms

> VSAM or Virtual Storage Access Method is a file storage access method used in
> MVS, ZOS, and OS/390 operating systems. It was introduced by IBM in 1970's.
> It's a high-performance access method to organize data as files in mainframes.
> VSAM is used by programming languages in mainframes to store and retrieve data

> VSAM files can be stored only on magnetic disk storage devices called as DASD
> (Direct Access Storage Device) space, requiring several cylinders to store the
> data which is not cost-effective

> VSAM files are used in mainframe programs (such as COBOL, PL/I, Assembler)
> like physical sequential files and within the programs, the records are either
> read sequentially like a flat file or randomly using keys to point to the
> specific record. There are four types of VSAM file systems which improves data
> storage and retrieval by overcoming certain restrictions that normally occurs
> while accessing conventional files like Sequential Files.
>
> - Key Sequence Data Set (KSDS) is the widely used where records are identified
>   with keys and any Data Manipulation operation could be done using the key
>   value.
> - Entry Sequence Data Set (ESDS) where records are kept in sequential order
>   and accessed as such.
> - Relative Record Data Set (RRDS) where record numbers are used as keys to
>   access records.
> - Linear Data Set (LSDS), is a byte-stream data set in a traditional z/OS
>   file. It is rarely used in applications.

- kind of a primitive RDBMS
- -- can't work outside mainframe
- -- relatively few people understand how to program them

### Endevor 

Looks like an attempt to do some kind of deployment pipeline and software
lifecycle management for mainframes.

https://www.broadcom.com/products/mainframe/application-development/endevor

> Endevor was designed to automate the development process, governing software
> change from the very first line of code through deployment and includes change
> tracking.

> Endevor integrates with multiple vendors to provide the unified deploy across
> enterprise platforms such as GitHub, GitLab, BitBucket, IBM DBB, COBOL, ISPW,
> Changeman and Dependency Based build. We invite you to take a comprehensive
> look at Endevor and Git, to help understand the benefits of an automated
> approach to securing and managing software assets, along with the ease of code
> deployment across enterprise DevOps.

### COBOL

- Common Business-Oriented Language a.k.a COBOL is a compiled English-like
  computer programming language designed for business use.
- It is imperative, procedural and, since 2002, object-oriented.
- It is primarily used in business, finance, and administrative systems for
  companies and governments.
- It is still widely used in legacy applications deployed on mainframe
  computers, such as large-scale batch and transaction processing jobs.

### JCL

- Job Control Language a.k.a JCL is the command language of Multiple Virtual
  Storage (MVS), which is the commonly used Operating System in the IBM
  Mainframe computers.
- JCL identifies the program to be executed, the inputs that are required and
  location of the input/output and informs the Operating System through Job
  control Statements.
- In mainframe environment, programs can be executed in batch and online modes.
- JCL is used for submitting a program for execution in batch mode.

### Multiple Virtual Storage (MVS)

> MVS was the prior name of what has become z/OS. What was started out as MVS in
> 1974 was renamed to:
>
> MVS/SP Version 1 MVS/XA Version 2 in 1981 MVS/ESA Version 3 (1988), Version 4
> (1991), and Version 5 (1994) OS/390 (1996) z/OS (2000)
>
> Along the way have come a plethora of new and enhanced functions. There are
> more differences than there is time to list them. However, we still call the
> "base control program" (BCP) "MVS" in many contexts, such as in the names of
> various z/OS books, to differentiate it from the other 70-ish parts
> ("elements") of z/OS.
>
> https://groups.google.com/g/bit.listserv.ibm-main/c/hZUymeB_azs/m/7-w_h91tjZMJ

Answer from an z/OS Technical marketing person

### z/OS

The modern mainframe operating system from IBM

### Zowe

- Zowe is an open source project that offers modern interfaces to interact with
  z/OS and allows you to work with z/OS in a way that is similar to what you
  experience on cloud platforms today.
- It is a modern solution for modern mainframe developers

### Zowe Explorer

- Zowe Explorer is a VSCode extension that allows you to access Zowe while using
  VSCode.
- This gives you the option to use IDEs rather than the 3270 terminals in
  developing your COBOL programs.

### HLASM

- High-level assembly language

### Code4z

- A VSCode extension pack for z/OS programming
- https://marketplace.visualstudio.com/items?itemName=broadcomMFD.code4z-extension-pack
- Includes coding helpers, debugger, Zowe etc.

### REXX (Restructured Extended Executor)

- a scripting language
- I think it's a kind of "powershell for mainframes" thing
- is apparently akin to perl or python

### RACF (Rack-eff Resource Access Control Facility)

- provides access control and auditing for z/OS and z/VM operating systems

## The filesystem

| Mainframe | Linux     |
| --------- | --------- |
| Library   | directory |
| Dataset   | File      |

- z/OS has a unique filesystem
    - z/OS can also create and manage "normal" unix filesystems
- it does not have a real filesystem like linux - you must to say how big files
  are going to be when created - it's more akin to creating a database table
- it is quite flat
- Partition dataset = a file which can contain other files (which are called
  members)
    - source code is usually in members within a PDS
- this is a **record oriented** operating system not a byte oriented one
    - => the OS understands the idea of records within files - files are not
      just blobs of bytes
    - the filesystem is quite primitive in some ways
- No files but has _Datasets_
    - Start with a _High Level Qualifier_ e.g. `USER1` part of `USER1.JOB.JCL`
    - Sequential = normal "files", dot naming convention e.g. `USER1.DATA`
    - Partitioned (PDS)
        - kind of like folders
        - datasets inside a PDS are called members e.g.
            - `USER1.PDS(FILE1)`
            - `USER1.PDS(FILE2)`
- terms
    - _data set_
        - Any named group of records is called a data set
        - In z/OS, there are no new line (NL) or carriage return and line feed
          (CR+LF) characters to denote the end of a record. Records are either
          fixed length or variable length in a given data set.
        - a file that contains one or more _records_
        - you must define the space allocated for the dataset at creation time
        - namign rules
            - max 44 UPPERCASE chars
            - divided by `.` into qualifiers (max 8 bytes per qualifier
            - High-level qualifier (HLQ) **may** be fixed by system security
              controls (e.g. set to your username)
            - but rest of qualifiers can be chosen by user
            - must not have the same name as any other data set in the system
              catalog
            - examples:
                ```sh
                AAAAAAA.AAAAAAAA.AAAAAAAA.AAAAAAAA.AAAAAAA # example of a max length data set name
                USER1234.AAAAAAAA.AAAAAAAA.AAAAAAAA.AAAAAAA # example with HLQ set to a username
                ```
        - things you need to find a data set
            1. data set name
            2. volume
            3. device type
        - things you need to find a data set if it has been cataloged
            1. data set name
        - z/OS treats a UNIX filesystem heirarchy as a collection of data sets
            - Each data set is a mountable filesystem
            - ??? Don't understand this yet
    - _System catalog_
        - a single logical function (which might store data across multipel
          catalogs, a master one and one per user)
        - most data sets are cataloged
    - _data set record_
        - the basic unit of information used by a processing program
        - almost all z/OS data processing is record-oriented
        - byte-streams are not present in traditional processing
        - but byte stream files are available through z/OS UNIX
    - PDS = Partitioned Data Set/ PDSE = ???
        - Also called z/OS _libraries_
        - PDSes contain _members_
        - Things which are stored in PDSes
            - Source programs
            - System params
            - Application control params
            - JCL
            - executable modules
    - VSAM = Virtual storage access method
        - allows for "complex" disk access
        - intended to be used by other programs, not editable via ISPF
    - ISPF = Interactive System Productivity Facility
        - the TUI app that lets you manage most z/OS functionality
    - DCB = ???
        - the group of attributes about a data-set. Includes RECFM, LRECL,
          BLKSIZE etc.
    - RECFM = Record Format
    - LRECL = Maximum logical record length
    - BLKSIZE = maximum block size
    - DFSMS
        - can create recipes for data sets
        - recipe includes
            - "performance goals"
            - data availability requirements
            - automate data backup
        - these recipes can be automatically applied to datasets when they are
          created

## Getting access to a mainframe environment to learn

### IBM z/OS Dev and Test (ZDT)

- https://www.ibm.com/docs/en/zdt/12.0.0?topic=personal-edition
- it requires an intel based CPU and a USB licence key.
- There is a _Learners edition_ available for USD 120/year (some hoop jumping
  required)
- Normal cost starts at ~$ 8500 USD/year

## z/OS Virtual Dev and Test (ZVDT)

- Released in mid 2022
- No USB key required
- IBM support running an emulated mainframe on a linux system
- This is called **ZVDT** _z/OS Virtual Dev and Test_
- You get the same software
- Has a web based interface to build and automate deployment of z/OS
  environments
    - you can create z/OS application images
- You need
    - sufficient grunt on the linux machine
    - 1 linux core per each virtual central Processor for each z/OS instance
    - 1 linux core allocated to underlying Linux
    - 1 GM for the underlying linux
    - 2-4 GB per z/OS instance
    - 4 GB for the thing that provides the web UI and sorts licensing
    - No physical media required, just downloads
- ZVDT is licensed by virtual CPs (RVUs) similarly to ZD&T
- https://www.ibm.com/common/ssi/ShowDoc.wss?docURL=/common/ssi/rep_ca/1/897/ENUS222-251/index.html

### IBM CLoud Wazi as a service

- details: https://www.youtube.com/watch?v=_QAEd1cBkWs
- Totally viable but **costs ~$5 USD/hour so can expensive quickly i.e. ~$3500
  USD/month!**
    - new IBM cloud account gets $200 in free credits
- https://www.ibm.com/docs/en/wazi-aas/1.0.0?topic=vpc-configurations-in-zos-stock-images
    - includes fairly up to date stuff (in early 2023)
        - Db2
        - COBOL for z/OS 6.4
        - Java v8 and v11 SDK for z/OS
        - Python 3.10 SDK for z/OS
        - Node v16 SDK for z/OS

### Zexplore

- https://www.ibm.com/z/resources/zxplore
- I don't think there is real mainframe access, I think it's just a series of
  quizes.

### Hercules + TK4-

You need:

1. An emulator (Hercules seems to be the only option)
2. An operating system (e.g. tk4-)

> Hercules Hyperion: an open source software implementation of the mainframe
> System/370 and ESA/390 architectures, in addition to the latest 64-bit
> z/Architecture.
>
> Hercules is a free emulator that runs on Linux, Windows, Solaris, FreeBSD, and
> Mac OS X.

I tried doing it in homebrew but it's annoying. macOS gatekeeper pings every
dynamic lib within hercules. In the end I just went with docker and it worked
first time.

- https://www.youtube.com/watch?v=uRf6A6_GWzw (guide)
- https://hub.docker.com/r/skunklabz/tk4-hercules (docker image)
- see `docker-compose.yml` in these notes for details

```bash
# terminal 1
laptop$ docker compose up

# terminal 2
laptop$ c3270 localhost:3270

# terminal 3
# Printers in TOS actually just text files in the ./prt directory
laptop$ docker compose exec mainframe bash
container$ cd ./prt && ls -l

# Browser
# A debugging/admin interface into Hercules - you can see the system log,
# registers etc. plus some buttons and dials I don't understand. Presumably the
# mimic the hardware on a real computer?
http://localhost:8038/
```

## Connecting to a mainframe

- You connect to a mainframe using a terminal emulator which can mimic a
  traditional 3270 terminal (the protocol used and the hardware buttons
  provided)
    - the 3270 protocol is a modified telnet with some stuff on top for the
      terminal (full list in man page) e.g. \*
      https://www.rfc-editor.org/rfc/rfc1576
        - https://www.rfc-editor.org/rfc/rfc2355

```bash
# install terminal emulator for macOS
brew install x3270

# c3270 is the ncurses based version

man c3270
# c3270 - curses-based IBM host access tool
#
# c3270 opens a telnet connection to an IBM host in a console window.  It
# implements RFCs 2355 (TN3270E), 1576 (TN3270) and 1646 (LU name selection), and
# supports IND$FILE file transfer.  If the console is capable of displaying
# colors, then c3270 emulates an IBM 3279.  Otherwise, it emulates a 3278.

# open a tn3270 session
laptop$ c3270 localhost:3270
```

### c3270 Tips

- Manual: http://x3270.bgp.nu/Unix/c3270-man.html
- Ctrl-] switch to the terminal's `c3270>` prompt (this sends commands to the
  terminal emulator, not to the connected machine)
- ctrl-h == backspace
- you have to be careful where you click your mouse
- click in a non editable area and you get an "X Protected" message
    - **use Tab to get to the next editable field** (this is the biggest
      usability tip)
- Use the "keymap" menu to enter the C3270 specific keys (Ctrl-k brings up the
  keymap screen)
    - can also be done on command line but I don't know/care how yet

Hercules TK4- default credentials for the most privileged account:

```
Username: herc01
Password: cul8tr
```

### TSO tips

- TSO is the mainframe shell
- TS4- works very similarly to the real IBM mainframe environment (apparently, I
  have no way to check this)
- The screens you see are called "panels"
- These keys are very important in a mainframe environment:
    - PF3 goes back up to the previous panel
        - PF3 is like a "back button" in the UI
        - PF3 maps to F3 on keyboard (make sure to switch laptop keys to the Fn
          keys)
    - PF8 (key F8) pages down (I think)
    - PF7 (key F7) pages up (I think)
    - F10 scroll left
    - F11 scroll right
    - F4 = quit
    - F1 = help
    - Screens will generally not refresh themselves but you can trigger a
      refresh with ENTER key
    - If you navigate down panels using numbers e.g. type 3, then type 4 you can
      shortcut by typing `3.4`
    - `TAB` moves forward to fields you can type
    - `Ctrl-A TAB` to "Back tab"
    - `Backspace` will consume types characters to the right of the cursor in a
      field
    - When on a dataset:
    - `e<Return>` will open it for editing
    - `b<Return>` will open it read only
    - e<Ret> while on a "file" opens it
- On the TSO apps screen, use RFE not RPF (RFE is much better apparently)
- IPL = mainframe for "booting up"
- Note: the compilers in tk4- are functional but ancient - 1960s era because the
  source code of those compilers had to be released.
- Tasks
    - Login
        - on first startup - if you are first login you have to hit `RESET` and
          then `CLEAR` to get the login prompt
    - To get to the full TSO prompt: Enter PF3 on the TSO applications menu
    - To logoff: Enter PF3 on the TSO applications menu then type `logoff` from
      the TSO prompt.
    - Shutdown: PF3 on the applications menu, then type `shutdown`, then
      `logoff`, then 30 secs later it will shutdown once your terminal emulator
      disconnects

## Many OSes

> Most of this information center teaches the fundamentals of z/OS®, which is
> IBM's foremost mainframe operating system. It is useful for mainframe
> students, however, to have a working knowledge of other mainframe operating
> systems. One reason is that a given mainframe computer might run multiple
> operating systems. For example, the use of z/OS, z/VM®, and Linux® on the
> same mainframe is common.

> In addition to z/OS, four other operating systems dominate mainframe usage:
> z/VM, z/VSE™, Linux for System z®, and z/TPF.

There are 5 important mainframe OSes

1. z/VM
    - the hypervisor that runs the other OSes as guests
    * can run any combination of guest OSes
    * components
        - Control Program CP
        - Single-user OS (CMS = Conversational Monitor System)
            - Run in a VM by the CP component
            - provides interactive interface and the z/VM API
1. z/OS
    - the "foremost" mainframe OS
    * You can have both your pre-prod z/OS systems and your prod z/OS on the
      same mainframe hardware
1. z/VSE z/Virtual Storage Extended
    - popular on smaller mainframes
    - was originally known as Disk Operating System (DOS)
    - first introduced in System/360 as a temporary measure until OS/360 was
      ready
    - smaller size than z/OS
1. Linux for System z
    - Many non-IBM distributions can be used on mainframe
    - IBM provide two:
        - Linux for S/390 (32 bit)
        - Linux for System z (64 bit)
    - Linux uses ASCII not EBCDIC
1. z/TPF z/Transaction Processing Facility
    - special purpose OS for very high transaction volume
    - once called Airline Control Program (ACP)
    - used by airlines and credit card companies
    - can use multiple mainframes in a loosely coupled environment to handle
      10's of thousands of transactions/sec

## CPUs vs other CPUS

- IBM Telum processor
    - 7 nm process
    - 8 cores per die, each at 5.2 GHz
    - each socket has a pair of dies so 16 cores per socket
    - each core has 256KB L1 cache and 32 MB L2 cache
        - when something needs to be evicted from a core's L2 cache it will try
          to find space for it in the L2 cache of another core and then mark it
          as "virtual L3 cache"
        - evicted cache lines can also be pushed to another chip in the system
          via proprietary fiber connections
    - integrated AI accelerator
    - https://arstechnica.com/gadgets/2021/09/ibms-telum-mainframe-processor-introduces-new-cache-architecture/
    - > Telum also introduces a 6TFLOPS on-die inference accelerator. It's
      > intended to be used for—among other things—real-time fraud detection
      > during financial transactions (as opposed to shortly after the
      > transaction).
    - released with z16
        - each frame designed to fit in 19" rack
        - can go up to 4 frames wide

## The case against mainframes

- IBM mainframes are just a more expensive way to run workloads as x86 has
  matured
    - sometimes orgs don't have the source of their custom software
- Talent scarcity
- A mainframe is not "one thing" - it runs many apps in LPARS (logical
  partitions)
- The systems are not "agile" - culturally or software or hardware
- Very high exit costs
    - Really strong backwards compatibility lets you keep your technical debt
      around
    - often no documentation, sometimes no source code
- Proprietary ecosystem
    - mainframe software isn't available outside mainframe e.g you can't get
      db2, CICS etc. for linux. This makes exiting a very complex rewrite - you
      have change the app code, the DB, the scheduler, the security etc. etc.
- Closed ecosystem - hard to shop around
- ecosystem gets more expensive over time (?? source)
- Anti-pattern migration strategies according to google
    1. rewrite all your apps
        - takes too long, hard for the rewrite to keep up
    1. run emulated mainframe
        - the hardware is really good, the lock-in is ecosystem layer and cost
          structure so running virtualised doesn't solve your actual problems
    1. modernise in place (keep the mainframe, update the software)
        - big-bang rewrite but worse
        - no new capabilities
        - still have problems with cost and talent scarcity
        - you don't get the cost benefit until you can get rid of the mainframe

## Migrating from mainframe to cloud

All the big public clouds have "mainframe transformation" teams and offerings.
Most of them have some sort of "assessment" step, aided by tools which will
parse your code and tell you what it connects to etc.

- Azure
    - https://www.youtube.com/watch?v=ymsG0yGU_VI
    - they call it "transform" rather than "migrate"
    - they focus
        - re-envision
            - no equivalent exists, find a different way to solve your business
              problem
        - re-host (emulate the mainframe env for your COBOL program)
        - refactor (rewrite in Java)
    - ~40% of mainframes are in financial services (according to an Azure
      presentation: https://youtu.be/ymsG0yGU_VI)
- AWS have a number of presentations about this
    - "legacy technology estate"
- Google G4 (semi-automated rewrite to Java)
    - Tools to parse your COBOL, PL/1, JCL etc.
    - Convert your code to Java
        - ??? how maintainable is this java?
    - Dashboard to let you monitor, test and debug the rewritten application

Case study z15 mainframe 190 processor cores at 5.2 GHz 40TB memory 85 LPARs (so
you can run up to 80 separate instances of machines on this)
