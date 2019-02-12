defmodule SpawnBasic do
  def greet do
    IO.puts("hello there")
  end
end

# * Aside: the word "term" is a catch-all to stand in for "any elixir value"

# self()
# ######

# * is a function not a macro
# * returns the pid of the calling process
# * is inlined by the compiler

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
# * its return value is the message you gave it as an argument

# registered names
# ################

# TODO

# message contents
# ################

# * messages can be elixir term but are usually atoms (if you need a simple flag) or tuples (they are usually tuples)
# * there is no special mechanism for sending the pid of the sender in a message - you have to include it in the tuple

# receive blocks
################

# * the current process will block waiting for messages when it executes `receive do ... end` so think of them as "receive and wait" blocks
# * are syntactically very similar to case blocks

defmodule Spawn1 do
  def greet do
    receive do
      {sender, msg} ->
        send(sender, {self, "hello #{msg}"})
    end
  end
end

pid = spawn(Spawn1, :greet, [])
# PID<0.1054.0>

send(pid, {self, "yo yo you"})
# {#PID<0.80.0>, "yo yo you"}
# note that send/2 returned the message

receive do
  {sender, msg} ->
    IO.puts("Got '#{msg}' from #{inspect(sender)}")
end

# immediate after we end the receive block the current process will start waiting for messages

# after
# #####

# * allows you to add a timeout to waiting to recieve a message
# * `after TIME_IN_MILLISEC ->`

# receive do
#   {sender, msg} ->
# after 500 ->
#   # do some stuff after the timeout
# end

# tail call optimization
# ######################

# * elixir has tail call optimization
# * GOTCHA: you have to be careful that the recursive call is really the last
#   line executed e.g. consider a factorial function

def factorial(0), do: 1
def factorial(n), do: n + factorial(n - 1)

# the second function head is really
def factorial(n) do
  x = factorial(n - 1)
  n + x
end

# ... which is not tail call optimized!

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

greeter = fn name -> IO.puts("Hello #{name}") end

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
# The name must be an atom, is often __MODULE__
@agent_name :some_atom
{:ok, pid} = Agent.start_link(fn -> Map.new() end, name: @agent_name)
Agent.update(@agent_name, fn state -> state.put(state, :value, thing) end)
value = Agent.get(@agent_name, fn state -> Map.get(state, :value) end)
Agent.stop(@agent_name)

# * uses a process to store a single value and let other processes access it
#     * that value can be arbitrarily complex but it is still a **single** value
# * agents provide two apis
#     1. anonymous functions
#         * requires both the client and agent have the same version of the
#           caller module so you have to account for this when doing rolling
#           upgrades.
#     2. uses as args: module, function, arguments
# * the Agent is the "server process" and your code is the "client process"
#     * you don't give it new values - you give it a function that **it** runs in
#       its own process that will mutate and/or return the value
#     * => don't do expensive things in the func you pass to the agent or it
#       will block the agent!
#
# * {:ok, pid} = Agent.start_link(anon_func_that_returns_starting_value, name: key_name)
# * value = Agent.get(key_name, func_that_gets_agent_state_as_arg_and_returns_a_value)
#     * sends the given function to the agent to run
#     * the result of the function is returned
# :ok = Agent.update(key_name, func_that_gets_value_as_arg_and_returns_new_value)
#     * sends function to the agent which runs it
#     * the function transforms the old state into the new state
# {old_state, new_state} = Agent.get_and_update(key_name, func
#     * gets the state and runs function to transform it to the new state in the
#       same call
