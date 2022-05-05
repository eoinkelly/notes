# A rack "app" == object that implements the interface:
#
#     app.call(env: Hash) -> [status-code: String, http-headers: Hash, body-lines: Array]
#
# * The body must respond to #each with Strings that successively represent the response body
#
require "rack"

##
# Rack app created with a lambda/proc
#
# proc_app = Proc.new do |env| # works
# proc_app = proc do |env| # works
proc_app = lambda do |env|
  [
    "200",
    {
      "Content-type" => "application/stuff"
    },
    [
      "this is line 1\n",
      "this is line 2\n"
    ]
  ]
end

##
# Rack app created within a class
#
class App
  def self.call(env)
    request = Rack::Request.new(env)
    [
      "200",
      {
        "Content-type" => "application/stuff"
      },
      [
        "this is line 1\n",
        "this is line 2\n"
      ]
    ]
  end
end

# Rack::Handler::WEBrick.run(proc_app) # works
Rack::Handler::WEBrick.run(App) # works

# TODO: couldn"t figure out how to choose puma as rack server in this situation
# require "puma"
# Rack::Handler::Puma.run(proc_app)