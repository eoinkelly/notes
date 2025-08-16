# Quickfix and location lists

Vim associates two "lists" with each open window

1. Quickfix List
    - shared between all windows
1. Location list
    - each window has a separate location list

The key to productive use of these lists is to learn to navigate through the
lists without changing to the window that opens

## Quickfix list

- Vim allows you to compile your program with an external tool and capture the
  output while staying in vim
- it puts the compiler output into the "quickfix" window
- The vim quickfix list is tied to the vim `:make` command

you can control what program gets called by :make with

    :set makeprg= ruby %

Commands

    :copen  # open the quickfix list
    :cclone # close quickfix  list
    :cnext  # next error
    :cprev  # previous error
    :cfirst # go to first error
    :clast  # go to last error
    :cc 3   # go to third error
    :cc     # see full error line
    :clist  # show only lines with file and line numbers
    :clist! # show all lines

Vim remembers the output of the previous 10 runs of the `make` program

    :colder # see an older run of the `:make` program
    :cnewer # see an newer run of the `:make` program

# Location list

Uses same commands as quickfix list except prefix with l not c e.g. `:lopen`,
`:cnext`
