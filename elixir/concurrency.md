# Concurrency

There are layers of concurrency API in Elixir (& Erlang)

1. Low-level process management:
    1. Kernel functions
    2. Process functions
2. Higher level abstractions: Task, GenServer, etc.

## Low-level process management

Most of the time we should use the OTP abstractions over this low-level process
management.

Releveant docs

- https://hexdocs.pm/elixir/Process.html
- https://hexdocs.pm/elixir/Kernel.html

```
# all these are auto-imported from Kernel
Kernel.spawn/1
Kernel.spawn/3
Kernel.spawn_link/1
Kernel.spawn_link/3
Kernel.spawn_monitor/1
Kernel.spawn_monitor/3
Kernel.self/0
Kernel.send/2
Kernel.exit(:some_msg)

receive do
end

Process functions
https://hexdocs.pm/elixir/Process.html
```

When two processes are linked, each one receives exit signals from the other.
There are a few variations of exit signal:

1. exit indicating normal finish
    - indicates a normal exit

    ```elixir
    # from the current process
    exit(:normal)                   # normal exit from any erlang process
    exit(:shutdown)                 # OTP normal exit from current process
    exit({:shutdown, some_term})    # OTP normal exit from current process
    exit({:shutdown, integer})      # Exit the CLI process this way and it will exit to shell with `integer` value

    # from another process
    Process.exit(pid, :normal)
    ```

1. exit indicating an error
    ```elixir
    exit(:some_reason) # from current process
    Process.exit(pid, :some_reason) # from another process
    ```

    - Can be converted into a message by setting a flag in the process
1. exit signal which cannot be converted into a message (untrappable exit)
    ```elixir
    # The method is the same from the current process and another process
    Process.exit(self(), :kill) # from current process
    Process.exit(pid, :kill) # from another process
    ```

Erlang also has _system messages_. You only need to care about them if you are
implementing an OTP compliant process from scratch. They have the form:

    {:system, from, request}

The only thing you need to do with them is pass them to
`sys:handle_system_msg(Request, From, Parent, Module, Deb, State)` and this
function calls one of your modules functions depending on what should happen:

    MyModule.system_continue
    MyModule.system_terminate

### Implementing an OTP compliant process from scratch

> http://erlang.org/doc/design_principles/spec_proc.html#special-processes
>
> System messages are messages with a special meaning, used in the supervision
> tree. Typical system messages are requests for trace output, and requests to
> suspend or resume process execution (used during release handling). Processes
> implemented using standard behaviours automatically understand these messages

A process started using start_link stores information (for example, about the
ancestors and initial call) that is needed for a process in a supervision tree.

If the process terminates with another reason than normal or shutdown, a crash
report is generated. For more information about the crash report, see the SASL
User's Guide.<Paste>

If you implement your own OTP compliant process then you need to call
`proc_lib:init_ack` in your init function

### SASL System Architecture Support Libraries

- http://erlang.org/doc/man/sasl_app.html

### Monitors

- Monitors are not links, they are a more relaxed way of knowing what happened
  to a process.
- Monitors are unidirectional and allow you to establish as many "monitors" as
  you want (remember how links limited the number of links between 2 processes
  to just 1)
- They use messages instead of signals,
    - these messages are not propagated like signals, so nothing happens to your
      process when a monitored process exits (except that you get a new message
      in your mailbox).

### Process dictionary

- Every process has a **private** map/dictionary available to it called the
  process dictionary
    - It seems like I cannot access the process dictionary of another process
      (Observer seems to be able to do this however so maybe its discouraged
      rather than impossible)
- you _could_ use it to make an Erlang process behave like an object in other
  languages but that is naughty and the Erlang community would hit you with a
  rolled up newspaper.
- Elixir Process module has functions for CRUDing data in the dictionary - see
  https://hexdocs.pm/elixir/Process.html
- Common process dictionary keys
    - There are a few keys which seem commonly used
        - `:"$ancestors"`
            - a List of Pids which are ancestors of this process
        - `:$initial_call`
            - seems to be a tuple representing the first function call
    - These keys seem to be written in by Erlang `proc_lib` to allow for crash
      debugging
    - Example
        ```
        '$initial_call' {supervisor,'Elixir.Supervisor.Default',1}
        '$ancestors' [<0.83.0>]
        ```

Don't add too much to the process dictionary because

- you no longer have referential transparency
- it gets copied whenever you run `Process.info(pid)` and memory usage will go
  up by that amount.
- Good summary
  http://erlang.org/pipermail/erlang-questions/2007-April/026330.html

> In OTP, the process dictionary is used for two purposes: storing the process'
> ancestors and also storing the initial function call. You can see them being
> inserted into your process every time you use proc_lib:spawn/1 or
> proc_lib:spawn_link/1:

> Other uses of the process dictionary include the random module, storing the
> random seed in the process. The user module uses it to fetch info from the
> shell process which might be waiting for results from somewhere else. The
> global module/server uses the process dictionary as a key-value cache for
> remote node configurations. The fprof application makes a similar use of it
> for its internal state.

Question: Does erlang process know which process created it?

- Under normal curcumstances there is no state stored in a process which tells
  it which process created it.
- You can sometimes guess which process did because of the presence of a link or
  monitor but that is not a guarantee

```
Process.put(key, value)
Process.delete(key)

iex> Process.get()
[
  "$initial_call": {IEx.Evaluator, :init, 4},
  iex_server: #PID<0.75.0>,
  "$ancestors": [#PID<0.75.0>],
  iex_evaluator: :ack,
  iex_history: %IEx.History{
    queue: {[
       {5, :"do not show this result in output"},
       {4,
        [#PID<0.0.0>, #PID<0.1.0>, #PID<0.2.0>, #PID<0.3.0>, #PID<0.4.0>,
         #PID<0.5.0>, #PID<0.6.0>, #PID<0.9.0>, #PID<0.41.0>, #PID<0.43.0>,
         #PID<0.45.0>, #PID<0.46.0>, #PID<0.48.0>, #PID<0.49.0>, #PID<0.51.0>,
         #PID<0.52.0>, #PID<0.53.0>, #PID<0.54.0>, #PID<0.55.0>, #PID<0.56.0>,
         #PID<0.57.0>, #PID<0.58.0>, #PID<0.59.0>, #PID<0.60.0>, #PID<0.61.0>,
         #PID<0.62.0>, #PID<0.63.0>, #PID<0.64.0>, #PID<0.65.0>, #PID<0.66.0>,
         #PID<0.67.0>, #PID<0.68.0>, #PID<0.69.0>, #PID<0.75.0>, #PID<0.79.0>,
         #PID<0.80.0>, #PID<0.81.0>, #PID<0.82.0>, #PID<0.83.0>, ...]},
       {3, nil},
       {2, :"do not show this result in output"}
     ],
     [
       {1,
        [:erts_code_purger, :init, Logger.BackendSupervisor, :erl_signal_server,
         :inet_db, Logger.Supervisor, :standard_error, :user, :elixir_sup,
         :elixir_config, :elixir_code_server, :logger_sup,
         :logger_handler_watcher, IEx.Config, :logger, :global_group,
         :erl_prim_loader, :standard_error_sup, IEx.Pry, Logger, :rex,
         :kernel_safe_sup, :kernel_sup, :kernel_refc, :user_drv,
         :global_name_server, IEx.Broker, :code_server, IEx.Supervisor,
         :file_server_2, :application_controller]}
     ]},
    size: 5,
    start: 1
  }
]
```

### Timed messages

What is a "timer ref"

```
Process.send_after/3
Process.cancel_timer/2
```

### Two-way link between processes with spawn_link()

- When you `spawn()` a new process B from process A then process A does not find
  out if anything happens to B
- If you use `spawn_link()` then you "join" the two proceses together.
  Consequences are
    - A will die if B dies abnormally
    - B will die if B dies abnormally
- linking is a two-way thing

Signals you can receive from a linked process:

1. exit(:reason)
1. error() aka exception
1. throw()

You can convert those "signals" into normal messages that your `receive ... do`
block can read by

```
# Run this in process A to trap the `exit` from **any** linked processes and turn it into a message of the form:
# {:EXIT, pid, :reason}
#
Process.flag(:trap_exit, true)
```

- `spawn_link()` is atomic so you don't have to worry about signals arriving
  before it finishes

spawn_link(ModuleName, :function_name, []) spawn_link(fn (args) -> ... end)

## One-way link between with spawn_monitor()

- `spawn_monitor()` run in AAA will
    - create a new process BBB
    - AAA will get a `{:DOWN, id_of_monitor_as_a_ref, pid, :reason}` message in
      its inbox if BBB exits or fails
    - BBB will not be aware of AAA

- `spawn_monitor()` is atomic so you don't have to worry about signals arriving
  before it finishes

## Stopping a process

There are two separate mechanisms for "abandon the current process" in Elixir

1. Raise an exceptions
    - avoid ending the process by using a `try ... rescue ... end` block
    - Question: what does raising an exception in a process send to linked
      processes? Anything?
2. Call `error()`, `exit()` or `throw()`
    - these "signals" will kill the current process and be forwarded on to all
      linked processes
    - they come from the erlang world
    - avoid ending the process by using a `try ... catch ... end` block

# Errors

- https://elixir-lang.org/getting-started/try-catch-and-rescue.html

In elixir there are three mechanisms for ending early

1. errors (aka exceptions)
2. throws
    - designed for flow control (only used in rare cases)
    - you can catch them with a `try ... catch`
    - if a `throw` happens outside a `try ... catch` it will raise an
      `** (ErlangError) Erlang error: {:nocatch, "the message"}` which will
      cause the current process to emit an exit signal with an error. That exit
      signal will be sent to all linked processes.
        - => so you cannot `throw` across process boundaries - if you try, it
          gets converted into an `ErlangError` exception and that in turn
          converted into a process exit
3. exit signals
    - aka `Kernel.exit/1`
    - stop the execution of the current process
    - call `exit(:normal)` if you want to end a process but not signal failure
    - call `exit(:anything_else)` if you want to
        1. end a process
        1. signal failure
        1. send EXIT to all linked processes (will make them crash unless they
           are trapping exits)
    - it has no return value because it causes the process to exit
    - OTP has three kinds of normal/polite process end
        1. `exit(:normal)`
        1. `exit(:shutdown)`
        1. `exit({:shutdown, term})` Anything else will be treated as a crash by
           OTP and the supervision will kick in.
    - you can catch them with a `try ... catch` but that is rare in practice -
      it is better to rely on the supervision tree.
    - use `exit({:shutdown, integer})` in a CLI to set the unix exit status
    - There are two varieties of exit signal
        1. trappable exit signal (you can convert these into messages)
        1. Untrappable exit signal (cannot be converted to messages)
            - used by `Process.exit(pid, :kill)`

### Exceptions

(this should maybe be moved to exceptions.exs later)

- raising an exception is fatal to the _process_ that it is raised in
- instead of rescuing them we usually let the process supervisor restart it
  again so you rarely see rescuing of exceptions in Elixir code

```elixir
raise "Kill my process"
# same as above
raise RuntimeError, message: "Kill my process"
```

```elixir
# will re-raise the current exception and **add** a message
# TODO: how does this work in more detail
reraise "some new message", System.stacktrace()
```

You can rescue errors using a `try ... rescue ... end` block. Rescuing errors is
not the recommended way to write Elixir

```elixir
try do
  # stuff
rescue
  [RuntimeError, FunctionClauseError] ->
    # do something specific
  error in [ArithmeticError] ->
    # do something else
  any_error ->
    # catch-all for errors not matched by patterns above
after
  # stuff that should always happen
end
```

### catch, exit, throw
