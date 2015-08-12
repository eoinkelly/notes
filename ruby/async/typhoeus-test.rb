require 'typhoeus'

# https://github.com/typhoeus/typhoeus

Typhoeus::Hydra.new(max_concurrency: 20) # default limit is 200
Typhoeus::Config.verbose = true

# we will get some improvements using a single hydra run to process each request but it would be even faster to have have a pool of hydra requests as ready as possible
#
url_1 = 'https://api.github.com/users/eoinkelly'
url_2 = 'https://api.github.com/repos/eoinkelly/angular-bootcamp'

# typhoeus wraps libcurl and defaults to infinite timeout
hydra_options = {
  timeout: 30, # seconds
  connecttimeout: 10 # seconds
}
hydra = Typhoeus::Hydra.hydra

first_request = Typhoeus::Request.new(url_1)

first_request.on_complete do |response|
  puts 'first request finished'

  # p response
  # third_url = response.body
  # third_request = Typhoeus::Request.new(third_url)
  # hydra.queue third_request
end

second_request = Typhoeus::Request.new(url_2)
second_request.on_complete do |response|
  puts 'second request finished'
  # p response
end

hydra.queue first_request
hydra.queue second_request

puts Time.now
hydra.run # this is a blocking call that returns once all requests are complete
puts Time.now
