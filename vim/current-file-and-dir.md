# Getting current file and dir on command line



```vim
; full path to the dir vim was opened with
:inoremap \fp <C-R>=getcwd()<CR>
;/Users/eoinkelly/Dropbox/Notes/on-github

; current filename without extension
:inoremap \fn <C-R>=expand("%:t:r")<CR>

; current filename with extension
:inoremap \fn <C-R>=expand("%:t")<CR>

; full path to current dir
:inoremap \fn <C-R>=expand("%:p:h")<CR>

; path to current dir relative to the dir vim was opened with
:inoremap \fn <C-R>=expand("%:h")<CR>
; vim
```
