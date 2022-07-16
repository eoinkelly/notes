require "pg"
require "securerandom"
# require "memory_profiler"


##
# Attempt 1:
# * I started using exec() instead of exec_params() when there were no params to bind. Is that bad practice?
# Outcomes
# * For each set of tuples we get from the DB we will allocate C mem and then Ruby mem
# * But the tuple set size can be controlled by the batch_size param so we can control memory usage in exchange for having to make more requests to the DB (we are sacrificing speed for memory usage control)
#
# Q: should i use exex_params here or one of th eother APIs. justify my choice
# 	if i used AR exec_query this would work for MySQL too maybe?
#
def fetch_w_cursor_attempt_1(conn, query_sql, binds: [], batch_size: 100)
	cursor_name = "eoin_cursor_#{SecureRandom.hex(5)}"

	setup_cursor_tuple_fraction_sql = "SET cursor_tuple_fraction TO 1.0"
	res_1 = conn.exec(setup_cursor_tuple_fraction_sql)
	res_1.clear

	# ON HOLD is what allows us to acccess this named cursor from future queries
	declare_cursor_sql = "DECLARE #{cursor_name} CURSOR WITH HOLD FOR #{query_sql}"
	res_2 = conn.exec(declare_cursor_sql)
	res_2.clear

	loop do
		fetch_sql = "FETCH #{batch_size} FROM #{cursor_name}";

		# allocates C memory for the returned tuples
		res = conn.exec_params(fetch_sql, binds)

		# copies the C memory tuples into Ruby heap (allocates a lot)
		ruby_mem_vals = res.values
		res.clear # clear the C mem now that we don't need it anymore

		puts "fetch_w_cursor: Got #{ruby_mem_vals.length} results"

		# yield the full batch of Array<Array> of values which are now in the Ruby heap.
		# We want to minimize memory allocations so we don't yield individual records which would require creating more new objects
		yield ruby_mem_vals

		# TODO: is this a good test to know when to finish? Is there a better test?
		if ruby_mem_vals.length < batch_size # we got a partial set of results so assume we are done
			break
		end
	end
ensure
	# No matter what happens in the method above we want to attempt to clean up the cursor
	# TODO: what happens if hte cursor doesn't exist?
	close_cursor_sql = "CLOSE #{cursor_name}"
	res_3 = conn.exec(close_cursor_sql)
	res_3.clear
end

def main
	db_name = ENV.fetch("DB_NAME")
	table_name = ENV.fetch("TABLE_NAME")

	conn = PG.connect(dbname: db_name)

	query_sql = "select * from #{table_name}"

	fetch_w_cursor_attempt_1(conn, query_sql, batch_size: 743) do |rows_batch|
		puts "main block: got #{rows_batch.length} results"
		# rows_batch.each do |row|
		#   operate on individual rows - remember that `row` will be a full copy of the row in the array so you will be allocating!
		# end
	end
end

main
