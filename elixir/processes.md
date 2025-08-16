# Processes and memory

- Elixir has no garbage collector
- Every process has its own heap
    - the data in your application is divided up between these processes
- When a process exits its heap is cleaned up which is how unused memory is
  deallocated
- => you should design your app in such a way that computations which need a lot
  of memory are in a separate process so that memory can be freed up after the
  computation finishes
- => process termination is how memory cleanup happens
    - is it the only way?

QUESTION: does phoenix run each request in a separate process?

In the erlang gui debugger

each process has a "group leader" pid list of pids it monitors list of pids it
is monitored by list of linked pids
