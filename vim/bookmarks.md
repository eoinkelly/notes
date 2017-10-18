# Vim Bookmarks

* https://github.com/kshenoy/vim-signature is pretty great
* marks are smart enough to stay valid even as you add/remove/edit lines

The scope of marks depends on their name

* `a-z` are valid within one file
    * (row, col)
    * they are removed when you close the buffer
    * are restored by undo and redo
    * can be used with operators e.g. `d't`
* `A-Z are aka "file marks", valid across all files
    * (row, col, filename)
    * can only be used with operators if you are in the same buffer as the mark
    * and suc
* `0-9` are numbered marks, set from .viminfo
    * cannot be set directly
    * 0 is the location of the cursor when you last exited vim
    * 1 is the location of the cursor the time before that

```
m{mark}  set mark {mark} (where {mark} = a-z)
`{mark}  go to the row and column for mark {mark}
'{mark}  go to column 0 of the row for mark {mark}

:marks      show marks
:delmarks {marks}     delete the specified marks (a-zA-Z0-9), special marks cannot be deleted
:delmarks!  delete all marks for current buffer

# special marks

'   cursor position before doing jump
"   cursor postiion when last exiting the current buffer
[   the first character of the previously changed or yunked text (within current file only)
]   the last character of the previously changed or yunked text (within current file only)
^   cursor position where the last time insert mode was stopped
.   cursor postion where the last change was made
>   first line or char (depending on whether you use ` or ') of last selected visual area (current buffer only)
>   last line or char (depending on whether you use ` or ') of last selected visual area (current buffer only)

# commands which are also available as marks (these are not listed by :marks for some reason)
# you can used these with operators within the buffer e.g. d(

(   start of current sentence
)   end of current sentence
{   start of current paragraph
}   end of current paragraph

[` or ['  go up to closest previously defined lowercase mark (a-z)
]` or ]'  go down to closest next defined lowercase mark (a-z)
```
