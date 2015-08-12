# A rack "app" == object that implements
# app.call(env: Hash) -> [status-code: String, http-headers: Hash, body-lines: Array]
# * The body must respond to #each with Strings that successively represent the response body
#
require 'rack'

# proc_app = Proc.new do |env| # works
# proc_app = proc do |env| # works
proc_app = lambda do |env|
  p env
  [
    '200',
    {
      'Content-type' => 'application/stuff'
    },
    [
      "this is line 1\n",
      "this is line 2\n"
    ]
  ]
end

##
#
class App
  def self.call(env)
    request = Rack::Request.new(env)
    [
      '200',
      {
        'Content-type' => 'application/stuff'
      },
      [
        "this is line 1\n",
        "this is line 2\n"
      ]
    ]
  end
end

Rack::Handler::WEBrick.run proc_app # works
# Rack::Handler::WEBrick.run App # works
