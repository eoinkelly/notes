# # Concurrency abstractions:
#
# 1. Tasks
#     * similar to futures in other langs
# * will only return to the process that called them
# 2. Agents
# * A simple abstraction over storing state
# * They run in a separate process and provide access to global state

## Task

greeter = fn name -> IO.puts "Hello #{name}" end


task1 = Task.async(fn -> greeter.("Eoin") end)
task2 = Task.async(fn -> greeter.("Amelia") end)
greeter.("Early riser")

Task.await(task1)
Task.await(task2)
