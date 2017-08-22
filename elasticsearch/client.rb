require "elasticsearch"
require "csv"
# require "json"

require "pry"


client = Elasticsearch::Client.new(url: "http://elastic:changeme@localhost:9200",
                                   log: false,
                                   reload_on_failure: true)

# client.transport.reload_connections!


CSV.foreach("tweets.csv", headers: true, header_converters: :symbol).each do |tweet|
  tweet_hash = tweet.to_h
  id = tweet_hash.delete(:id)

  tweet_hash[:posted_at] = Time.parse(tweet_hash[:posted_at]).iso8601

  begin
    if client.get(index: "tweets", type: "tweet", id: id)
      print "."
      next
    end
  rescue Elasticsearch::Transport::Transport::Errors::NotFound
    puts "rescuing"
  end

  puts "  Creating #{id}"
  client.create(index: "tweets", type: "tweet", id: id, body: tweet_hash)
end

puts "Done"
