require 'pg'

# Output a table of current connections to the DB
conn = PG.connect(dbname: 'eoin_play')
conn.exec("SELECT * FROM pg_stat_activity") do |result|
  puts "     PID | User             | Query"
  result.each do |row|
    # puts row
    puts " %7d | %-16s | %s " % row.values_at('pid', 'usename', 'application_name')
  end
end
