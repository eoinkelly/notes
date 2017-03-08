# Zsh

* anytime you are doing stuff at the prompt in zsh you are interacting with the
  _Zsh line editor_ or _zle_ for short
    * so your the text you type on the line and any shortcuts all go to the
      shell - when you hit return the shell
        1. parses the text you just entered
        2. figures out which commands to run
        3. backgrounds itself and runs those commands, plumbing their stdin, stdout, stderr as you have instructed in your command
* zsh is very close in functionality to ksh

## Things the shell provides

* globbing ("global replacement")
* completion
* aliases for common commands
* a full scripting environment
* ability to run commands from
    * other binaries
    * commands built-in to itself (called "builtins")
    * functions and aliases defined by you in startup files

## Ways of invoking zsh

```zsh
# 1.
zsh # no script filename given so zsh starts an "interactive" shell for you to type cmds

# 2.
zsh foo.sh # zsh starts a non-interactive shell and runs the commands in `foo.sh`

# 3.
zsh -l # tell zsh to start as a "login shell"

# 4.
- zsh # starts zsh in login shell - this weird syntax is how the unix "login" command invokes shells
```

The main difference between login and interactive shells

1. different sets of startup files are sourced for each kind of shell
1. you can type 'logout' to exit a login shell (`exit` will exit all shell types)

Aside: `bye` is an alias for `exit` but isn't standard across shells so
probably not worth remembering.

The order of startup scripts on mac is

```plain
Starting .zshenv (run for all zsh shells)
Finished .zshenv
Starting /etc/zprofile (run for login shells)
Ending /etc/zprofile
Starting .zprofile (run for login zsh shells only)
Finished .zprofile
Starting /etc/zshrc
Finished /etc/zshrc
Starting .zshrc (run for interactive shells only)
Finished .zshrc
Starting .zlogin (run when entering a zsh login shell)
Finished .zlogin
```

The reason that both `.zprofile` and `.zlogin` exist is that they run before and after `.zshrc` which gives you options for customising the shell when you don't control the environment or all the startup scripts

## Scripting environment provide by zsh

* zsh calls variables "parameters" which isn't at all confusing, no siree
* you can use single quotes or double quoates
    * single quotes
        * ++ they escape everything including newlines up to next single quote

```zsh
print hi there # works without quotes
print "hi there" # also works
print $ZSH_VERSION # use print to inspect values

# use -- to indicate that anything which follows is not an option so you can
# print strings which begin with a dash
print -- -23 -foo -bar

# -R tells print to only recognize BSD style options
print -R

foo="hello"
print foo # => foo
print $foo # => hello

# single quotes disables expansion
print '$foo' # => foo

# double quotes allow expansion
print "$foo" # => hello

# [[ whatever ]] is a special builtin command
if [[ -o multibyte ]]
then
print "This zsh supports multibyte"
else
print "This zsh does not support multibyte"
fi


# parameter assignment (notice no space either side of =)
foo="hello there"

# parameter (variable) expansion is triggered by `$`
# expansion is also called "substittion"
print $foo

# you can always use {} in expansion - zsh is not always required but helps
# makes things clearer
print ${foo}


# arrays
# assigned with ()
foo=(hello there world)

# arrays are 1 indexed
# expanded with ${foo[idx]}
print ${foo[1]} # => hello

# you only use parens when defining a function, not invoking it
# define the function
# you cannot pass args to a function using () - it must always appear as ()
do_thing() {
    print "hi there"
}
do_thing # invoke the function
```

## Options

* zsh options are boolean
* can be written upper or lower case e.g. `foobar` is same as `FOOBAR` is same as `FoOBaR`
* can have any number of `_` in them e.g. `FOOBAR` is same as `FOO_BAR` is same as `_F_OO_B_AR_`
* most options should be set in `.zshrc` so they only affect interactive shells - you might get weird side effects if they also set for scripts
* many options also have single letter variants

zsh provides a simple way to inspect current options:

```
$ setopt # show options which are set
$ set -o # does same as setopt, compatible with ksh (note set without -o passes positional params and does not set options i.e. is is totally different)
$ unsetopt # show options which are unset
```


## Startup files on macs

```zsh
# contents of /etc/zshrc on a mac

##
# * if you are running zsh in Apple Terminal app then ???
#
if [ "$TERM_PROGRAM" = "Apple_Terminal" ]; then
  setopt combiningchars
fi

##
# * "disable" disables zsh stuff
# * this command disables the zsh "log" builtin so that /usr/bin/log will work
# * /usr/bin/log allows you to access system wide log messages created by os_log,
#   os_trace and other logging systems
disable log
```

```
# /etc/zprofile on macs

# The path_helper utility reads the contents of the files in the directories
# /etc/paths.d and /etc/manpaths.d and appends their contents to the PATH
# and MANPATH environment variables respectively.  (The MANPATH environment
# variable will not be modified unless it is already set in the environ-
# ment.)
# -s tells path_helper to output bourne shell not csh style path

# system-wide environment settings for zsh(1)
if [ -x /usr/libexec/path_helper ]; then
  eval `/usr/libexec/path_helper -s`
fi
```


## signals

Ctrl-c sends SIGINT
if you start a background job and then exit the shell before the job finishes zsh will send SIGHUP to the job process unless you set the NO_HUP option

If NO_HUP is set then zsh won't send SIGHUP to the background job

QUESTION: does that mean I could use this to leave a job running on a server I login to without using `screen` or `tumx`?
