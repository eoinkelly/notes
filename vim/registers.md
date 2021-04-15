# Vim Registers

```bash
:registers  # show registers
:display    # show registers
:di         # show registers
:reg        # show registers

:reg x      # show register x (filters the output of :reg)
```

## Available registers

* `"0` = yanked text goes in here
* `"1` = the large delete register
* `"2` to `"9` = large (one full line or more) deletions register stack,
    * entries get moved down and then out as you add new entries by changing or deleting text
    * **yanked text never goes into these!**
* `"-` = small (less than full line) deletions register
* `"a` to `"z` = normal named registers
* `"A` to `"Z` = same normal named registers but appends to them instead of overwriting
* `""` = default unnamed register, used when you don't name a register. gets changed whenever any other register except "_ is changed
* `"_` = black hole register, always empty
* `".` = last inserted text (read only)
    * use it to insert the same text multiple times
* `"%` = current file path (starting at dir where vim was opened) (read only)
* `"#` = alternate file register (read only, not aboslute path, unsure of details)
* `":` = command register (read only, the last command run, will be `:register` if you used that command to view the list)
* `"/` = search register (the last thing you searched for)
*     * can be pasted into ex commands using `ctrl-r\`
* `"=` = expression register (presumably the last vimscript expression run)
*     * can be used to execute expressions and insert them
* `"*` = usually the OS clipboard. On XWin it goes to the middle-click pasteboard
* `*+` = the OS clipboard on all platforms
* `"~` = drop register. The last text that was drag & dropped. Only available in Gvim

```bash
# From insert mode, paste the named reg
Ctrl-r <regname>
```

## Consequences

* `p` will paste from the `""` register by default.
* When you yank text, it goes into `"0` **and** `""` by default.
* When you delete less than a line it goes into `"-` and `""` by default
* When you delete (or change via a `c` command) a line or more it goes into `"1` and `""` by default and gets moved through `"2` through `"9`
* You can paste a register into a search command or other command in `:ex` mode using `Ctrl-r`
* There is no stack of yanked text - there is only one yank register.
    * You can yank text into the named registers to manage it yourself
* => you can always paste the last deleted thing from `""` without having to yank it
* if you specify a custom register then the normal one will not be used (except `""` I think)
* undoing a delete does not remove it from the delete register stack

## Macros

Macros are recorded into a register too. `qa` records into the `a` register
=> macros can be saved to a text file!


## Strategies for using registers efficiently

```bash
# Option:

y<textobj> # yank the yext to copy (into "" and "0)
# move to new place
va<textobj> # select what to replace
p # paste from ""


# Option:

y<textobj> # yank the yext to copy (into "" and "0)
# move to new place
c<textobj> # delete the old text and enter insert mode (puts old text in "" and "1
Ctrl-r0 # paste from "0 (which was not changed by the delete)
```