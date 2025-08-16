# COBOL

- https://www.youtube.com/watch?v=TBs7HXI76yU ( have not watched this yet but
  looks good)
- https://gnucobol.sourceforge.io/HTML/gnucobpg.html
    - functional reference and user guide for gnu cobol

## Installation

```sh
$ brew install gnu-cobol

# commands installed by gnu-cobol
$ cobc
$ cob-config
$ cobcrun
```

As of 2023-01-31 the VSCode COBOL extension causes VSCode to crawl so I only
enable it while working on COBOL

## Overview

Source:
http://opencobol.add1tocobol.com/guides/GNU%20COBOL%202.0%20Programmers%20Guide.pdf

Programs have 4 "divisions"

1. Identification division
2. Environment division
3. Data division
4. Procedure division

Each division can contain 1+ "sections" Each section can contain 1+ "paragraphs"
Each paragraph can contain 1+ "sentences"

Types of section

1. Organisation section
2. Screen section
    - used to put stuff on a terminal screen

## Fixed format mode

Fixed format is somewhat legacy and designed around 80 column punch cards.

The columns are:

1-6: sequence no. area _ reserved for 6 digit sequence no. on punch cards - so
if you dropped your cards on the floor you could put them into a "card sorter"
and it would fix them for you based on this number _ ignored by compiler 7:
indicator area _ 5 possible values: 1. space _ nothing indicated about this
line 1. - _ continuation character (so a literal from previous line could be
continued) 1. d,D,/,_ \* are all comments indicators

- 8-11: Area A
    - things which must begin in Area A:
    - language division headers (division, section, paragraph)
    - FD, SD, SORT description headers
    - "level numbers" 01, 77
- 12-72: Area B
    - things which must begin in Area A:
    - most of your program lives here
- 73-80: Program name area
    - anything past col 73 is ignored by compiler
    - used for an 8 char program name on punch cards, again so the card sorter
      could sort them if you dropped them

## Free format

- no constraints on columns
- compiler will read up to 256 columns
- Gnu COBOL compiler defaults to fixed mode. Turn it off with either:
    1. `--free` command line switch
    2. `>>SOURCE FORMAT IS FREE` statement beginning at column 8 at the top of
       program
- NOTE: comments ceated with `*>` not `*` in free format
- Indentation helps the human reader, but COBOL does not care about indentation,
  it is strictly by closest previous position in the source file when
  determining which field an 88 level conditional is dependent on.
- > Notice that in the examples above I've sometimes used a hyphen, and other
  > times used an underscore. COBOL does not differentiate between these two
  > characters
    - I have found this to be true in most cases but some keywords do care

## Comments

```cobol
*> In free format COBOL the comment is *>
*> In fixed format COBOL the comment is * in col 7
```

## Compiler directing facility CDF

- sets flags for the compiler

```cobol
        >>SOURCE FORMAT IS FREE
        >>SOURCE FORMAT IS FIXED
```

## Syntax

- doesn't distinguish between `-` and `_` in keywords but does in register names

## Structured data

- "elementary items" = data that cannot be broken down further (akin to integer,
  float etc.)
- "group items" = data that can be broken down (akin to array, dictionary etc.)

all/some??? lines end in `.`

## Money

Cobol supports fixed-point arithmetic which is good for managing money and
avoids the pitfall of using floats for money

Q: is this superior to just storing the number of cents?

https://en.wikipedia.org/wiki/Fixed-point_arithmetic

## Screens

Apparently you can make TUI screens quite easily in Cobol

# Importing code from other files (Copybooks

> A Copybook is a segment of program code that may be utilized by multiple
> programs simply by having those programs use the COPY statement to import that
> code. This code may define files, data structures or procedural code.

> Today’s current programming languages have a statement (usually, this
> statement is named “import”, “include” or “#include”) that performs this same
> function. What makes the COBOL copybook feature different than the “include”
> facility in newer languages, however, is the fact that the COPY statement can
> edit the imported source code as it is being copied.

# CDF Compiler Directing Facility

- A way to put compiler flags in your source code.
- seems to also have functionality like the C preprocessor but
- a mix of compiler flags and text manipulation statements

> The Compiler Directing Facility, or CDF, is a means of controlling the
> compilation of GnuCOBOL programs. CDF provides a mechanism for dynamically
> setting or resetting certain compiler switches, introducing new source code
> from one or more source code libraries, making dynamic source code
> modifications and conditionally processing or ignoring source statements
> altogether. This is accomplished via a series of special CDF statements and
> directives that will appear in the program source code.

## Standard library

- Called _Intrinsic functions_ in COBOL
- Docs: https://gnucobol.sourceforge.io/HTML/gnucobpg.html#ABS

- Built-in system subroutines
    - https://gnucobol.sourceforge.io/HTML/gnucobpg.html#C_0024CALLEDBY
        - > There are a number of built-in system subroutines included with
          > GnuCOBOL. Generally, these routines are intended to match those
          > available in Micro Focus COBOL, ACUCOBOL and directly for GnuCOBOL.
          > It is recommended to change the CBL_OC routines to CBL_GC for
          > forward compatibility as at some point they will be removed as they
          > are a hangover from Open Cobol.
    - they seem to have evolved from a primitive "system call alike" interface
      that Micro Focus COBOL had
        - they also include
            - some string manipulation functions
            - shelling out to other programs
            - boolean logic comparisons
        - I _think_ the grouping characteristic is that this is a mimic of a
          very old API. Presumably the intrinsic functions fill in the gaps.
          Makes the stdlib a bit messy but :shrug:
        - they can have a number of prefixes which seem to act as a namespace
            - "C$"
            - "CBL\_"
            - "MF\_"
        - > Early versions of Micro Focus COBOL allowed programmers to access
          > various runtime library routines by using a single two-digit
          > hexadecimal number as the entry-point name. These were known as
          > call-by-number routines. Over time, Micro Focus COBOL evolved,
          > replacing most of the call-by-number routines with ones accessible
          > using a more conventional call-by-name technique. Most of the
          > call-by-number routines have evolved into even more powerful
          > call-by-name routines, many of which are supported by GnuCOBOL. Some
          > of the original call-by-number routines never evolved call-by-name
          > equivalents; GnuCOBOL supports some of these routines.
    - These routines, all executed via their upper-case names via the CALL
      statement (see CALL), are capable of performing the following functions:
        - Changing the current directory
        - Copying files
        - Creating a directory
        - Creating, Opening, Closing, Reading and Writing byte-stream files
        - Deleting directories (folders)
        - Deleting files
        - Determining how many arguments were passed to a subroutine
        - Getting file information (size and last-modification date/time)
        - Getting the length (in bytes) of an argument passed to a subroutine
        - Justifying a field left-, right- or center-aligned
        - Moving files (a destructive “copy”)
        - Putting the program “to sleep”, specifying the sleep time in seconds
        - Putting the program “to sleep”, specifying the sleep time in
          nanoseconds; Caveat: although you’ll express the time in nanoseconds,
          Windows systems will only be able to sleep at a millisecond
          granularity
        - Retrieving information about the currently-executing program
        - Submitting a command to the shell environment appropriate for the
          version of GnuCOBOL you are using for execution

## Style recommendations

> Avoid the use of level 77 data items in new programs. Once (1968 and before)
> there were valid reasons for creating level-77 data items, but since the 1974
> ANSI standard of COBOL there really hasn’t been any reason why an elementary
> level-01 data item couldn’t have been used instead of a level-77 item.
