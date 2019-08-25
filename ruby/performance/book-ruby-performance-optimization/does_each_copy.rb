require "memory_profiler"

num_rows = 100_000
num_cols = 10

puts "%d MB" % (`ps -o rss= -p#{Process.pid}`.to_i / 1024)
GC.disable



b = nil
report = MemoryProfiler.report do
  a = Array.new(num_rows) { Array.new(num_cols) { "x" * 1000 } }
  b = Array.new(num_rows) { Array.new(num_cols) { "x" * 1000 } }
end

report.pretty_print(scale_bytes: true)
puts "%d MB" % (`ps -o rss= -p#{Process.pid}`.to_i / 1024)
