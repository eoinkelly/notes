
defmodule SpawnBasic do
  def greet do
    IO.puts "hello there"
  end
end

# * Aside: the word "term" is a catch-all to stand in for "any elixir value"

# self()
# ######

# * is a function not a macro
# * returns the pid of the calling process
# * inlined by the compiler

# spawn
# #####
#
# * spawn(func) makes a new process from an anonymous function
# * spawn(module, func_name_atom, args_list) makes a new process from a named function in a module
# * when we spawn a new process we do not know exactly **when** it will execute
# * spawn creates a new process from a **function** i.e. each process has a single function at its top
# iex(316)> pid = spawn(SpawnBasic, :greeter, [])


# send
# ####

# * `send <DESTINATION>, <MESSAGE>`
# * destination can be
#     * local pid
#     * remote pid
#     * local port
#     * a locally registered name
#     * a `{registered_name, node}` tuple for a registered name at another node
# * is inlined by the compiler


# registered names
# ################

# TODO


# message contents
# ################

# * messages can be anything but are usually atoms or tuples
# * there is no special mechanism for sending the pid of the sender in a message
#   - you have to include it in the tuple


# receive blocks
################

# * the current process will block waiting for messages when it executes `receive do ... end` so think of them as "receive and wait" blocks
# * are syntactically very similar to case blocks

defmodule Spawn1 do
  def greet do
    receive do
      {sender, msg} ->
        send sender, {self, "hello #{msg}"}
    end
  end
end


pid = spawn(Spawn1, :greet, [])
#PID<0.1054.0>

send pid, {self, "yo yo you"}
# {#PID<0.80.0>, "yo yo you"}
# note that send/2 returned the message

receive do
  {sender, msg} ->
    IO.puts "Got '#{msg}' from #{inspect sender}"
end
# immediate after we end the receive block the current process will start waiting for messages

# Concurrency abstractions
# ########################
#
# 1. Tasks
#     * similar to futures in other langs
#     * will only return to the process that called them
# 2. Agents
#     * A simple abstraction over storing state
#     * They run in a separate process and provide access to global state

# Task
# ####

greeter = fn name -> IO.puts "Hello #{name}" end

task1 = Task.async(fn -> greeter.("Eoin") end)
task2 = Task.async(fn -> greeter.("Amelia") end)
greeter.("Early riser")

Task.await(task1)
Task.await(task2)

# Agent
# #####
#
# * You want to avoid expensive operations in the function you pass to the agent
#   as it will block the agent until the request is fulfilled
# * uses same name registration rules as GenServer

# Create an agent that holds a single Map value
@agent_name :some_atom # must be atom, often __MODULE__
Agent.start_link(fn -> Map.new end, name: @agent_name)
Agent.update(@agent_name, fn (state) -> state.put(state, :value, thing) end)
Agent.get(@agent_name, fn (state) -> Map.get(state, :value) end)
Agent.stop @agent_name

