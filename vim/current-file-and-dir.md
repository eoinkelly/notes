# Getting current file and dir on command line

## command mode (after `:`)

Ctrl-R then % will paste the path (relative to vim starting dir) to current file onto the command line


## insert mode

Ctrl-R then % will paste the path (relative to vim starting dir) to current file into the file the the cursor

## normal mode

Ctrl-G " display path in status line
1 then Ctrl-G` will display the full path to the file in status line

## general

* the % register contains the relative (to vim start dir) path of the current file
* register # contains the name of the alternate file.
* you can paste from these registers just like any other register

For example, in directory /abc the command vim def/my.txt would edit file
/abc/def/my.txt. The following commands could be entered to display the
information shown.

:echo @%                def/my.txtdirectory/name of fi  le
:echo expand('%:t')     my.txtname of file ('tail')
:echo expand('%:p'  )   /abc/def/my.txtfull path
:echo expand('%:p:h')   /abc/defdirectory containing file ('head')

The following commands insert lines consisting of the full path of the current
and alternate files into the buffer:

:put =expand('%:p')
:put =expand('#:p')


Some example shortcuts

```vim

" Make %% expand (on the command line) into the containing path of the currently open buffer
cnoremap %% <C-R>=expand('%:h').'/'<cr>

" full path to the dir vim was opened with
:inoremap \fp <C-R>=getcwd()<CR>
"/Users/eoinkelly/Dropbox/Notes/on-github

" current filename without extension
:inoremap \fn <C-R>=expand("%:t:r")<CR>

" current filename with extension
:inoremap \fn <C-R>=expand("%:t")<CR>

" full path to current dir
:inoremap \fn <C-R>=expand("%:p:h")<CR>

" path to current dir relative to the dir vim was opened with
:inoremap \fn <C-R>=expand("%:h")<CR>
" vim
```
