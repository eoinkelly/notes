require 'rack'

# create the rack app object
# app = Proc.new do |env|
#   ['200', {'Content-Type' => 'text/html'}, ['a first line', 'another line']]
# end

class MinimalRack
  def call(env)
    ['200', {'Content-Type' => 'text/html'}, ['a first line', 'another line']]
  end
end
# Give the rack compatible app to a rack aware web server
Rack::Handler::WEBrick.run MinimalRack.new.call
