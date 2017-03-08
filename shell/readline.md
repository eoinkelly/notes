# Readline

* a unix lib which provides a single line editor
* used by bash, irb (optionally) and others
* Customized with ~/.inputrc

Cheatsheet: http://readline.kablamo.org/emacs.html

* Readline like shortcuts are somewhat in common with macOS shortcuts

meta = alt key

```
# Very useful shortcuts I don't know
meta-/ = show possible tab completions
C-_ = undo last action
C-x C-e = edit the current line in vim
```

```
# movement
C-a = start of line
C-e = end of line
C-f = forward one char
C-b = backward one char
Meta-b = backward one word
Meta-f = forward one word

# delete
C-k = kill to end of line
C-u = kill to start of line

C-w = delete word backwards
Meta-d = delete word forwards

# undo
C-_ = undo last action
C-x C-u = undo last action

# paste
C-y = paste last killed text
Meta-_ = paste last argument of previous cmd

# transpose
Meta-t = transpose two chars
C-t = transpose two words

# misc
C-l = clear screen
C-r = reverse incremental search
meta-/ = show possible tab completions
```
