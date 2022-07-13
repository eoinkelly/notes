require "active_record"

##
# A toolkit class to wrap the best bits of the Rails API for dealing with raw SQL
class SaferRawSqlToolkit
  include ActiveRecord::Sanitization
  # adds
  # .sanitize_raw_sql

  def self.mk_binds(vals_ary)
    vals_ary.map do |val|
      #
    end
  end
  # very WIP

end

class SaferRawSql
  # The formats refer to the format of message
  # There are two formats a client can use to exchange messages with a PostgreSQL server (text or binary).
  # TEXT_FORMAT = 0
  # BINARY_FORMAT = 1

  ##
  # You must be using the PostgreSQL database to use this method because it
  # relies on PostgreSQL features.
  #
  # This method returns the same results as `ActiveRecord::Base.execute` but it
  # helps you avoid SQL injection attacks by using the PostgreSQL feature which
  # allows you to send the SQL code and values separately to the database.
  #
  # This means you don't have to rely on stanitization to avoide SQL
  # injections. Santization is helpful but limited in many cases e.g. it is
  # difficult to effectively sanitize strings representing human names.
  #
  # If you need to tell Postgres what type a particular parameter should be
  # treated as, you should tell it in the SQL e.g.
  #
  #     ... WHERE template = $1::string
  #
  # Example:
  #
  #     sql = "SELECT * FROM pages WHERE template = $1"
  #     # sql = "SELECT * FROM pages WHERE template = $1::string" # same query with explict type cast
  #
  #     params = ["topics"]
  #     results = SaferRawSQL.safer_execute(sql, params)
  #     p results
  #     => [{"id"=>13,
  #          "title"=>"Topics",
  #          "slug"=>"topics",
  #          "label"=>"Topics",
  #          "order"=>2,
  #          "template"=>"topics",
  #          "show_in_nav"=>true,
  #          "created_at"=>"2017-05-04 12:00:00.251916",
  #          "updated_at"=>"2017-05-04 12:00:00.331536"}]
  #
  # @param sql [String] SQL query which references parameters as $1, $2 etc.
  # @param param_values [Array<Any>] list of parameter values. $1 in the SQL
  #   references the first element, $2 the second etc.
  #
  # @return [Array<Hash<String, Any>>] Array of Hash results from the query
  def self.safer_execute(sql, param_values)
    # Transform `param_values` so it is suitable for passing to `PG::Connection#exec_params`.
    #
    #
    # values = param_values.map do |param|
    #   {
    #     value: param,
    #     # format: TEXT_FORMAT
    #   }
    # end

    # Get the PG::Connection instance that the `pg` gem provides
    pg_conn = ActiveRecord::Base.connection.raw_connection
    # => pg_conn.class == PG::Connection
    # https://www.rubydoc.info/gems/pg/PG/Connection#sync_exec_params-instance_method

    # Use that pg connection object to invoke `exec_params`
    pg_result = pg_conn.exec_params(sql, param_values)

    # this large array copy isn't ideal :-(
    results = pg_result.to_a
    pg_result.clear

    results
  end

  # wraps a given query in a PREPARE EXECUTE and uses
  # ActiveRecord::Base.connection.execute to execute it
  def self.exec_wrapped(sql_query, params)
    sql_query = "#{sql_query};" unless sql_query.ends_with?(";")

    stmt_name = "a#{Time.now.to_i}"
    unrolled_params = params.map { |param| "'#{param}'" }.join(",")

    prepared_sql = <<~EO_SQL
      PREPARE #{stmt_name} AS #{sql_query}
      EXECUTE #{stmt_name}(#{unrolled_params});
    EO_SQL

    pg_result = ActiveRecord::Base.connection.execute(prepared_sql)

    ActiveRecord::Base.connection.execute("DEALLOCATE #{stmt_name};")

    results = pg_result.to_a

    # Clear the memory associated with the PG::Result
    pg_result.clear

    results
  end

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
  def self.execute_sql(my_sql)
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
end


def main
  results = SaferRawSql.execute_sql("SELECT etc. etc.")
  SaferRawSql.execute_sql("SELECT etc. etc.") do |results|
    # ...
  end
end

main
