# Subprocesses

Sources

- Ruby Tapas ep 414, 389

Ruby has ??? ways to spawn sub processes

## system

- is mostly a version of `Kernel.spawn` that automatically waits for the child
  to exit
- blocking: yes
    - ruby will block until subprocess completes
    - unsuitable for long running processes
- file descriptors
    - inherited from parent
        - subprocess stdout and stderr will go to same place as the parent
          process (your ruby script)
- implicit subshell
    - ruby will auto detect shell syntax e.g. IO redirection and will switch to
      running the bare command to running it in the system default shell
    - you can avoid the implicit subshell by passing args as an array
    - see ep 389 rubytapas
    - different OSes have differnt commands built-in to shell so you can
      accidently spawn a shell sometimes e.g. `system("echo", "$PATH")` may
      spawn a shell if `echo` is a shell built-in on your system (it is on
      windows, mac but not on linux)
    - this detection is done by `Kernel.exec`
- controlling environment
    - can set new env variables via env hash
    - can unset existing env vars for the subprocess by setting values to nil in
      the env hash
    - can use the options hash to not only pass explicitly set env vars to the
      child
- control process name in listings
    - if you pass two element array instead of string for the process name e.g.
      `["cmd", "argv0"]` then the second element will be passed as `argv[0]` in
      the process so it will become the name of the process in various listings
        - TODO: which ones?
- options
    - set process group
    - set resrouce limits
    - set umask
    - clear env vars except those explicitly passed
    - redirect file descriptors
        - the standard ones have special symbol names e.g. `:in`, `:out
        - other FDs use their integer
    - change current working dir of the process
- return value
    - returns true if process exited with success (zero)
    - returns false if process existed with error status (non zero)
    - returns nil if command execution failed
    - the exit status of the last command passed to the subprocess is available
      in `$?` or `$CHILD_PROCESS` (if you have `require "english"`)

```ruby
# system(optiona_env_hash, command_string_or_array_of_strings, optional_options_hash)

system "say 'hello'" # spawns the command directly

system "ls -l 1>/dev/null 2>&1" # spawns a sh which will then run the command

system "echo 'Time to make some $$!'" # ruby sees shell special var $$! so spawns implicit subshell
system "echo", "Time to make some $$!" # no implicit subshell

# if first arg is hash it is assumed to be environment variables override

system({"HOME" => "/tmp"}, "cd ~; pwd")
```

```
$ sudo dtruss -f ruby -e'system "ls -l"'
dtrace: failed to execute ruby: dtrace cannot control executables signed with restricted entitlements
doesnt work on mac - TODO: find out how to get this working
```

### Aside: $? (aka $CHILD_PROCESS)

- assigned automatically by ruby
- is an instance of Process::Status

```
[16] pry(main)> $CHILD_STATUS.methods - Object.methods
=> [:&,
 :>>,
 :to_i,
 :success?,
 :pid,
 :stopped?,
 :stopsig,
 :signaled?,
 :termsig,
 :exited?,
 :exitstatus,
 :coredump?]
```

### Aside: Process.detach

Some operating systems retain the status of terminated child processes until the
parent collects that status (normally using some variant of wait()). If the
parent never collects this status, the child stays around as a zombie process.
Process::detach prevents this by setting up a separate Ruby thread whose sole
job is to reap the status of the process pid when it terminates. Use detach only
when you do not intend to explicitly wait for the child to terminate.

# spawn

- seems to be the most low-level way to spawn subprocesses
- will not block by default
    - you must either wait for it via `Process.wait` or explicitly ignore it via
      `Process.detach`
    - if you don't do one of these the OS may accumulate zombie processes
        - QUESTION: why?
- uses `Kernel.exec` under the hood to actually run the process
    - `Kernel.exec` uses the exec(2) system calls directly

```
pid = spawn("tar czvf ./abc")
Process.wait(pid)
```
