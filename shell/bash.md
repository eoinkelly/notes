# Bash

Sources

* https://tldp.org/LDP/abs/html/fto.html
* https://mywiki.wooledge.org/BashGuide
* https://mywiki.wooledge.org/BashPitfalls

It is a *convention* that your login name and home directory are the same but it could be different. IIRC `/etc/passwd` is consulted to decide what to use as the cwd of your login shell process.

Changing working dirs with `cd` is actually changing the working dir of your shell process

## Tilde expansion

* `~`
    * is provided by bash and has a few roles
        1. expands to the home dir of a user - any characters immediately after it (ended by a `/`) are considered part of the login name
        1. `~` (without any chars after it) will expand to whatever your $HOME is -
           if you change $HOME then `~` changes too
        1. if you append `+`, `-`, and some numbers it will expand to paths on the `dirs` stack
    * https://www.gnu.org/software/bash/manual/html_node/Tilde-Expansion.html
`-` is provided by bash as the previous working directory of the process

## Pathname expansion

Expand strings to files and directories **which exist**. Pathname expansion does **not** generate arbitrary strings for you

There are 4 wildcards:

1. `*` Any string of characters (inluding empty string)
1. `?` Any single character
1. `[set]` Any character in `set`
    * can also use range syntax e.g. `[a-Z0-9]`
1. `[!set]` Any character not in `set`
    * can also use range syntax e.g. `[!a-Z0-9]`

The name for the process of converting paths with wildcards into full paths is "wildcard expansion" or "globbing"

QUESTION: what about `**`?

When you use pathname expansion all the expansion is done by the shell - the command gets a big list of paths as if you had typed them out manually. For example:

```
$ cat test
#!/usr/bin/env ruby
p ARGV

$ ./test hi there
 ["hi", "there"]

$ ./test /usr/local/*
  ["/usr/local/Caskroom", "/usr/local/Cellar", "/usr/local/Frameworks", "/usr/local/Homebrew", "/usr/local/MacGPG2", "/usr/local/bin", "/usr/local/etc", "/usr/local/foreman", "/usr/local/heroku", "/usr/local/include", "/usr/local/lib", "/usr/local/mysql-utilities-1.5.3", "/usr/local/opt", "/usr/local/remotedesktop", "/usr/local/sbin", "/usr/local/selenium", "/usr/local/share", "/usr/local/texlive", "/usr/local/var"]
```

## Brace expansion

* allows you to generate arbitrary strings


```bash
$ echo b{ed,olt,ar}s
beds bolts bars

# braces can be nested
$ echo b{ar{d,n,k},ed}s
bards barns barks beds

$ echo file{a..f}
filea fileb filec filed filee filef

$ echo file{1..11}.txt
file1.txt file2.txt file3.txt file4.txt file5.txt file6.txt file7.txt file8.txt file9.txt file10.txt file11.txt

# brace expansion can be used with wildcards
$ ls *.{c,o,rb}
 sample.rb  temp.c  temp.o
$ ./test *.{c,o,rb}
  ["temp.c", "temp.o", "sample.rb"]
```
## Finding where and what commands are

In bash

* `which`
    * separate binary
    * searches $PATH for the given arg
    * `which -a somebin` will keep searching even after it finds the first match
* `type`
    * shows what kind of thing the arg is (alias, function, command etc.)
    * `help type`
    * `type -a thing` shows all matches
* `where`
    * not available in bash
* `whence`
    * not available in bash

In zsh

* `whence`
    * built-in to zsh
    * whence -f `foo` will show the defintion of `foo` if it is a shell function
    * `whence -aS thing` is a good forumla as it
        * returns all matches
        * show symlinks including all intermediate symlinks if there are any
* `where`, `which`, `type`
    * aliases for the zsh built-in `whence` with different option flags applied

In both shells

* `whereis`
    * separate binary
    * searches for programs in common directories
    * doesn't take any options on mac so there is no reason to use it over the shell built-ins
    * on other unixen including linux it can be used to search for binaries,
      source code and man pages
        ```
        linux$ whereis ls
        ls: /bin/ls /usr/share/man/man1/ls.1.gz
        ```
* `locate`
    * separate binary
    * searches a database of file paths for a matching path
    * that DB must be re-computed regularly
    * DB is in `/var/db/locate.database` on macOS
    * The locate.updatedb utility updates the database used by locate(1).  It
      is typically run once a week by the
      `/System/Library/LaunchDaemons/com.apple.locate.plist` job. The contents
      of the newly built database can be controlled by the `/etc/locate.rc`
      file.
    * On mac `/etc/locate.rc` is all comments but it seems to contain most of
      the directories on the system so is usable.

## Escaping

`\` at the end of a line works because it is quoting the linefeed character you entered by hitting RETURN

# Readline editing (emacs mode)
