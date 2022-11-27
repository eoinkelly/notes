##
# This (very) rough ruby script unpacks the ES slow query log lines a bit to
# make them easier to understand. The slow query log includes defaults like
# `boost: 1.0` and `adjust_pure_negative: true` which can be quite distracting
# in a deeply nested query. This script removes those from the query.
#
# The output is JSON with comments so pipe stdout into a .jsonc file to get
# syntax highlighting
#
# Usage:
#
#     docker compose logs --no-log-prefix --tail 0 -f elasticsearch | ruby slow_query_log_filter.rb > output.jsonc
#
require "json"

def remove_distracting_defaults(container)
  if container.instance_of?(Hash)
    container.delete("boost") if container["boost"] == 1
    container.delete("adjust_pure_negative") if container["adjust_pure_negative"] == true

    container.each_key do |key|
      remove_distracting_defaults(container[key]) if container[key].instance_of?(Hash) || container[key].instance_of?(Array)
    end
  end

  if container.instance_of?(Array)
    container.each do |el|
      remove_distracting_defaults(el) if el.instance_of?(Hash) || el.instance_of?(Array)
    end
  end
end

ARGF.each_line do |line|
  log_line_hsh = JSON.parse(line)
  next unless log_line_hsh["type"] == "index_search_slowlog"

  src = JSON.parse(log_line_hsh["source"])
  remove_distracting_defaults(src)
  pretty_src = JSON.pretty_generate(src)

  puts "// #{"X" * 80}"
  puts <<~EO_OUT
    // timestamp: #{log_line_hsh["timestamp"]}
    // took: #{log_line_hsh["took"]}
    // total_hits: #{log_line_hsh["total_hits"]}
    // search_type: #{log_line_hsh["search_type"]}
    // index searched: #{log_line_hsh["message"]}
    // source:
    #{pretty_src}

  EO_OUT
end
