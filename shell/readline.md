# Readline

- a unix lib which provides a single line editor
- used by bash, irb (optionally) and others
- Customized with ~/.inputrc
- Readline like shortcuts are somewhat in common with macOS shortcuts
- Cheatsheet: http://readline.kablamo.org/emacs.html

Ways of entering the "meta" key e.g. `Meta-b` on macOS

1. Press `Esc` then release then press `b`
1. Hold `Option` down and press `b`
1. Press `Ctrl` and `[` together and then release and then press `b`

```
# Very useful shortcuts I don't know
Ctrl-_ = undo last action (Ctrl-- seems to work just fine too)
Ctrl-x Ctrl-e = edit the current line in vim
```

```
# movement
Ctrl-f = forward one char
Meta-f = forward one word
Ctrl-e = forward to end of line

Ctrl-b = backward one char
Meta-b = backward one word
Ctrl-a = backward start of line

# delete
Ctrl-d = delete forward one char
Meta-d = delete forward one word
Ctrl-k = delete forward to end of line

Del         = delete backwards one char
Ctrl-h      = delete backwards one char
Meta-Del    = delete backwards one word
Meta Ctrl-h = delete backwards one word
Ctrl-w      = delete backwards one word

Option-h will open the man page for the command and bring you right back to the command afterwards - neat!

Ctrl-u = delete backwards to start of line

# undo
Ctrl-_ = undo last action
Ctrl-x Ctrl-u = undo last action

# paste
Ctrl-y = paste last killed text
Meta-_ = paste last argument of previous cmd

# transpose
Meta-t = transpose two chars
Ctrl-t = transpose two words

# misc
Ctrl-l = clear screen
Meta-/ = show possible tab completions
Ctrl-x Ctrl-e = edit the current line in vim
Tab = show matching tab completions (seems better than Meta-/)

# history (not sure if the history stuff is in readline or the shell?)
Ctrl-n = next line of history
Ctrl-p = previous line of history
Ctrl-r = search backwards
Esc < = move to first line of history
Esc > = move to last line of history
```
