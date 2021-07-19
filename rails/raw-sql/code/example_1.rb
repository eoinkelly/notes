##
# A function for running raw SQL in a Rails app.
#
# Note that it may be much more memory efficent to directly use the
# `PG::Result` instance and then call `clear` rather than copying it into an
# array like we do here.
#
# The method shown here does **nothing** to prevent or mitigate SQLi - you need
# to manage it yourself.
#
# Use this function for inspriation not copy & paste :-)
#
# @param [String] my_sql String
def execute_sql(my_sql)
  pg_result = ActiveRecord::Base.connection.execute(my_sql)

  # In this example we are just calling #to_a to convert the PG::Result to an
  # Array. PG::Result has a nice API for slicing and dicing itself so you may
  # want to to something clever instead. See
  # https://www.rubydoc.info/gems/pg/PG/Result for details.
  #
  # The important bit here is that we are copying all the data we care about
  # out of the PG::Result in preparation for later clearing the PG::Result
  results = pg_result.to_a

  # Calling #clear on the PG::Result is the important bit of cleanup and the
  # whole reason this method exists. See
  # https://www.rubydoc.info/gems/pg/PG/Result#clear-instance_method
  pg_result.clear

  yield results if block_given?

  results
end

# Then invoke it like this
results = execute_sql("SELECT etc. etc.")

# or

execute_sql("SELECT etc. etc.") do |results|
  # ...
end