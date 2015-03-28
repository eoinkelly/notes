# COBOL

Source: http://opencobol.add1tocobol.com/guides/GNU%20COBOL%202.0%20Programmers%20Guide.pdf

Programs have 4 "divisions"

1. Identification division
2. Environment division
3. Data division
4. Procedure division

Each division can contain 1+ "sections"
Each section can contain 1+ "paragraphs"
Each paragraph can contain 1+ "sentences"

Types of section

1. Organisation section
2. Screen section
    * used to put stuff on a terminal screen

## Fixed format mode

* designed around 80 column punch cards

cols

1-6: sequence no. area
    * reserved for 6 digit sequence no. on punch cards - so if you dropped your
      cards on the floor you could put them into a "card sorter" and it would
      fix them for you based on this number
    * ignored by compiler
7: indicator area
    * 5 possible values:
        1. space
            * nothing indicated about this line
        1. -
            * continuation character (so a literal from previous line could be
              continued)
        1. d,D,/,*
            * are all comments indicators
* 8-11: Area A
    * things which must begin in Area A:
    * language division headers (division, section, paragraph)
    * FD, SD, SORT description headers
    * "level numbers" 01, 77
* 12-72: Area B
    * things which must begin in Area A:
    * most of your program lives here
* 73-80: Program name area
    * anything past col 73 is ignored by compiler
    * used for an 8 char program name on punch cards, again so the card sorter
      could sort them if you dropped them

## Free format

* no constraints on columns
* compiler will read up to 256 columns
* Gnu COBOL compiler defaults to fixed mode. Turn it off with either:
    1. `-free` command line switch
    2. `>>SOURCE FORMAT IS FREE` statement beginning at column 8 at the top of program

## Compiler directing facility CDF

* sets flags for the compiler
```cobol
        >>SOURCE FORMAT IS FREE
        >>SOURCE FORMAT IS FIXED

```

## Structured data

* "elementary items" = data that cannot be broken down further (akin to integer, float etc.)
* "group items" = data that can be broken down (akin to array, dictionary etc.)

all/some??? lines end in `.`
