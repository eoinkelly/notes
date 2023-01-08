# Ruby debug gem

* installed by default in ruby 3.1+
* supports back to Ruby 2.6
* supports VSCode integration
* create break
  ```ruby
  binding.break
  binding.b
  debugger
  ```
* you can run a ruby script under `rdbg myfile.rb` and it will drop into the debugger after an exception is raised
* has ability to record and replay
  * TODO: can this be used in rails?
* is faster than other debuggers - No performance penalty on non-stepping mode and non-breakpoints.
* supports remote debugging
* is the default in Rails 7

### My experience with debug vs pry-byebug

++ is for ways it debug is better than pry-byebug, -- is for the reverse

* ++ debug has color output in backtraces
* ++ Backtrace in debug contains values of method call or block arguments
* ++ de bug has single letter commands by default
* -- new command shortcuts to learn
* ++ debug shows you the first two lines of backtrace when you drop into the debugger - handy
* ++ very expressive ways to drop into debugger - can watch ivars, stop on lines or class#method or based on conditional logic
* -- I had to install the iterm2 shell integration to make mouse scrolling work in debug (this might just be an iterm thing)
* ++ debug supports running things under Procfile like Rails 7 wants to do by default

```
# key commands
s # step
n # next
l # list - @ in pry-byebug
c # continue. is this same as !!@  ?

is there a equivalent of !!!

```

```
(rdbg) h    # help command

### Control flow

* `s[tep]`
  * Step in. Resume the program until next breakable point.
* `s[tep] <n>`
  * Step in, resume the program at `<n>`th breakable point.
* `n[ext]`
  * Step over. Resume the program until next line.
* `n[ext] <n>`
  * Step over, same as `step <n>`.
* `fin[ish]`
  * Finish this frame. Resume the program until the current frame is finished.
* `fin[ish] <n>`
  * Finish `<n>`th frames.
* `c[ontinue]`
  * Resume the program.
* `q[uit]` or `Ctrl-D`
  * Finish debugger (with the debuggee process on non-remote debugging).
* `q[uit]!`
  * Same as q[uit] but without the confirmation prompt.
* `kill`
  * Stop the debuggee process with `Kernel#exit!`.
* `kill!`
  * Same as kill but without the confirmation prompt.
* `sigint`
  * Execute SIGINT handler registered by the debuggee.
  * Note that this command should be used just after stop by `SIGINT`.

### Breakpoint

* `b[reak]`
  * Show all breakpoints.
* `b[reak] <line>`
  * Set breakpoint on `<line>` at the current frame's file.
* `b[reak] <file>:<line>` or `<file> <line>`
  * Set breakpoint on `<file>:<line>`.
* `b[reak] <class>#<name>`
   * Set breakpoint on the method `<class>#<name>`.
* `b[reak] <expr>.<name>`
   * Set breakpoint on the method `<expr>.<name>`.
* `b[reak] ... if: <expr>`
  * break if `<expr>` is true at specified location.
* `b[reak] ... pre: <command>`
  * break and run `<command>` before stopping.
* `b[reak] ... do: <command>`
  * break and run `<command>`, and continue.
* `b[reak] ... path: <path>`
  * break if the path matches to `<path>`. `<path>` can be a regexp with `/regexp/`.
* `b[reak] if: <expr>`
  * break if: `<expr>` is true at any lines.
  * Note that this feature is super slow.
* `catch <Error>`
  * Set breakpoint on raising `<Error>`.
* `catch ... if: <expr>`
  * stops only if `<expr>` is true as well.
* `catch ... pre: <command>`
  * runs `<command>` before stopping.
* `catch ... do: <command>`
  * stops and run `<command>`, and continue.
* `catch ... path: <path>`
  * stops if the exception is raised from a `<path>`. `<path>` can be a regexp with `/regexp/`.
* `watch @ivar`
  * Stop the execution when the result of current scope's `@ivar` is changed.
  * Note that this feature is super slow.
* `watch ... if: <expr>`
  * stops only if `<expr>` is true as well.
* `watch ... pre: <command>`
  * runs `<command>` before stopping.
* `watch ... do: <command>`
  * stops and run `<command>`, and continue.
* `watch ... path: <path>`
  * stops if the path matches `<path>`. `<path>` can be a regexp with `/regexp/`.
* `del[ete]`
  * delete all breakpoints.
* `del[ete] <bpnum>`
  * delete specified breakpoint.

### Information

* `bt` or `backtrace`
  * Show backtrace (frame) information.
* `bt <num>` or `backtrace <num>`
  * Only shows first `<num>` frames.
* `bt /regexp/` or `backtrace /regexp/`
  * Only shows frames with method name or location info that matches `/regexp/`.
* `bt <num> /regexp/` or `backtrace <num> /regexp/`
  * Only shows first `<num>` frames with method name or location info that matches `/regexp/`.
* `l[ist]`
  * Show current frame's source code.
  * Next `list` command shows the successor lines.
* `l[ist] -`
  * Show predecessor lines as opposed to the `list` command.
* `l[ist] <start>` or `l[ist] <start>-<end>`
  * Show current frame's source code from the line <start> to <end> if given.
* `edit`
  * Open the current file on the editor (use `EDITOR` environment variable).
  * Note that edited file will not be reloaded.
* `edit <file>`
  * Open <file> on the editor.
* `i[nfo]`
   * Show information about current frame (local/instance variables and defined constants).
* `i[nfo] l[ocal[s]]`
  * Show information about the current frame (local variables)
  * It includes `self` as `%self` and a return value as `%return`.
* `i[nfo] i[var[s]]` or `i[nfo] instance`
  * Show information about instance variables about `self`.
* `i[nfo] c[onst[s]]` or `i[nfo] constant[s]`
  * Show information about accessible constants except toplevel constants.
* `i[nfo] g[lobal[s]]`
  * Show information about global variables
* `i[nfo] ... /regexp/`
  * Filter the output with `/regexp/`.
* `i[nfo] th[read[s]]`
  * Show all threads (same as `th[read]`).
* `o[utline]` or `ls`
  * Show you available methods, constants, local variables, and instance variables in the current scope.
* `o[utline] <expr>` or `ls <expr>`
  * Show you available methods and instance variables of the given object.
  * If the object is a class/module, it also lists its constants.
* `display`
  * Show display setting.
* `display <expr>`
  * Show the result of `<expr>` at every suspended timing.
* `undisplay`
  * Remove all display settings.
* `undisplay <displaynum>`
  * Remove a specified display setting.

### Frame control

* `f[rame]`
  * Show the current frame.
* `f[rame] <framenum>`
  * Specify a current frame. Evaluation are run on specified frame.
* `up`
  * Specify the upper frame.
* `down`
  * Specify the lower frame.

### Evaluate

* `p <expr>`
  * Evaluate like `p <expr>` on the current frame.
* `pp <expr>`
  * Evaluate like `pp <expr>` on the current frame.
* `eval <expr>`
  * Evaluate `<expr>` on the current frame.
* `irb`
  * Invoke `irb` on the current frame.

### Trace

* `trace`
  * Show available tracers list.
* `trace line`
  * Add a line tracer. It indicates line events.
* `trace call`
  * Add a call tracer. It indicate call/return events.
* `trace exception`
  * Add an exception tracer. It indicates raising exceptions.
* `trace object <expr>`
  * Add an object tracer. It indicates that an object by `<expr>` is passed as a parameter or a receiver on method call.
* `trace ... /regexp/`
  * Indicates only matched events to `/regexp/`.
* `trace ... into: <file>`
  * Save trace information into: `<file>`.
* `trace off <num>`
  * Disable tracer specified by `<num>` (use `trace` command to check the numbers).
* `trace off [line|call|pass]`
  * Disable all tracers. If `<type>` is provided, disable specified type tracers.
* `record`
  * Show recording status.
* `record [on|off]`
  * Start/Stop recording.
* `step back`
  * Start replay. Step back with the last execution log.
  * `s[tep]` does stepping forward with the last log.
* `step reset`
  * Stop replay .

### Thread control

* `th[read]`
  * Show all threads.
* `th[read] <thnum>`
  * Switch thread specified by `<thnum>`.

### Configuration

* `config`
  * Show all configuration with description.
* `config <name>`
  * Show current configuration of <name>.
* `config set <name> <val>` or `config <name> = <val>`
  * Set <name> to <val>.
* `config append <name> <val>` or `config <name> << <val>`
  * Append `<val>` to `<name>` if it is an array.
* `config unset <name>`
  * Set <name> to default.
* `source <file>`
  * Evaluate lines in `<file>` as debug commands.
* `open`
  * open debuggee port on UNIX domain socket and wait for attaching.
  * Note that `open` command is EXPERIMENTAL.
* `open [<host>:]<port>`
  * open debuggee port on TCP/IP with given `[<host>:]<port>` and wait for attaching.
* `open vscode`
  * open debuggee port for VSCode and launch VSCode if available.
* `open chrome`
  * open debuggee port for Chrome and wait for attaching.

### Help

* `h[elp]`
  * Show help for all commands.
* `h[elp] <command>`
  * Show help for the given command.
```