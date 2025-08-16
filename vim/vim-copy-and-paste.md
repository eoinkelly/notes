# Vim copy & paste

gp paste and move cursor to end of what you pasted p paste last
yanked/changed/deleted thing just after the cursor P paste last
yanked/changed/deleted thing just before the cursor pastes with the original
indentation ]p does a `p` paste at the level of indentation the cursor is
currently at [p does a `P` paste at the level of indentation the cursor is
currently at

:reg show registers list

0 register = default for yanks " register = default for changes, deletes, yanks

when you yank it is put in both 0 and " when you delete/change it goes into "
only

`*` is the system clipboard register

- ???

```
":   reg
"%   app/models/beemo_cache_manager.rb
"#   /usr/local/share/vim/vim74/doc/change.txt

There are ten types of registers:			*registers* *E354*

1. The unnamed register ""
2. 10 numbered registers "0 to "9
2. register 0
  register 1-91
    * they form a stack of the previous 9 full line or more deletes/yanks/changes
    * if the delete/change is less than one line it doesn't go into these registers!
    * first delete goes into "" and "1 (i.e.  deletes skipe"0)
3. The small delete register "-
    * gets the output of commands that delete less than one line
4. 26 named registers "a to "z or "A to "Z
5. three read-only registers ":, "., "%
    * ". is the last inserted text - very handy for multiple pastes
    * ": is the last ex command I ran
    * "% is the current file path and is always available
6. alternate buffer register "#
    * "# is the alternate buffer path (it only exists if there is an alternate file)
7. the expression register "=
8. The selection and drop registers "*, "+ and "~
9. The black hole register `"_
    * like /dev/null for registers
10. Last search pattern register "/
```

Macros are recorded into registers!!!

running a macro is like pressing the exact sequence of keys in that macro's
register.
