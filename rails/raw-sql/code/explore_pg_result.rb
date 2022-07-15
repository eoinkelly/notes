require "active_record"
require "pg"
require "memory_profiler"

db_name = ENV.fetch("DB_NAME")
table_name = ENV.fetch("TABLE_NAME")

conn = PG.connect(dbname: db_name)

# res.inspect # => "#<PG::Result:0x0000000102a67040 status=PGRES_TUPLES_OK ntuples=10 nfields=15 cmd_tuples=10>"
# res.clear
# res.inspect # => "#<PG::Result:0x0000000102a67040 cleared>"

# Testing PG::Result#values
# #########################

# Test 1:
########
# Allocates 17.65 MB total
# report = MemoryProfiler.report do
# 	res  = conn.exec(%Q(select * from #{table_name)) # allocates 17.65 MB
# end
# report.pretty_print(scale_bytes: true)

# Test 2:
########
# Allocates 44.9 MB total
# report = MemoryProfiler.report do
# 	res  = conn.exec(%Q(select * from #{table_name})) # allocates 17.65 MB
# 	xx = res.values # allocates 27.25 MB
# end
# report.pretty_print(scale_bytes: true)

# Test 3:
########
# Allocates 70.15 MB total
report = MemoryProfiler.report do
	res  = conn.exec(%Q(select * from #{table_name})) # allocates 17.65 MB
	ar_res = ActiveRecord::Result.new(res.fields, res.values) # allocates 27.25 MB (for the 2nd arg)
	example_ary = ar_res.to_a # allocates  25.01 MB (because it builds a hash from the res.values we passed in
end
report.pretty_print(scale_bytes: true)

# Conclusion: PG::Result#values allocates significant memory, up to 1.5x the memory used by PG::Result to hold the tuples

# Q: is there any way to get the data out of a PG::Result wihtout paying that cost?
# A: I don't think there is. You just have to be aware.