# minimal rack app
#
# run this by
#
# $ cd path/to/this/files/dir
# $ rackup

require 'pry'

# NOTE: {} has higher precedence than do...end so if you use do...end then you need to invoke #run with parens
# Invoking with {} seems to be more common in the docs
run(lambda do |env|
  # env.class => Hash
  # env =
  # {
  #   "rack.version": [
  #     1,
  #     6
  #   ],
  #   "rack.errors": "#<Rack::Lint::ErrorWrapper:0x000000014b04be78>",
  #   "rack.multithread": true,
  #   "rack.multiprocess": false,
  #   "rack.run_once": false,
  #   "rack.url_scheme": "http",
  #   "SCRIPT_NAME": "",
  #   "QUERY_STRING": "",
  #   "SERVER_PROTOCOL": "HTTP/1.1",
  #   "SERVER_SOFTWARE": "puma 5.6.4 Birdie's Version",
  #   "GATEWAY_INTERFACE": "CGI/1.2",
  #   "REQUEST_METHOD": "GET",
  #   "REQUEST_PATH": "/",
  #   "REQUEST_URI": "/",
  #   "HTTP_VERSION": "HTTP/1.1",
  #   "HTTP_HOST": "localhost:9292",
  #   "HTTP_USER_AGENT": "curl/7.79.1",
  #   "HTTP_ACCEPT": "*/*",
  #   "puma.request_body_wait": 0,
  #   "SERVER_NAME": "localhost",
  #   "SERVER_PORT": "9292",
  #   "PATH_INFO": "/",
  #   "REMOTE_ADDR": "127.0.0.1",
  #   "puma.socket": "#<TCPSocket:0x000000015ba82b48>",
  #   "rack.hijack?": true,
  #   "rack.hijack": "#<Proc:0x000000014b0582e0 /Users/eoinkelly/.rbenv/versions/3.0.3/lib/ruby/gems/3.0.0/gems/rack-2.2.3/lib/rack/lint.rb:567>",
  #   "rack.input": "#<Rack::Lint::InputWrapper:0x000000014b04bf68>",
  #   "rack.after_reply": [

  #   ],
  #   "puma.config": "#<Puma::Configuration:0x000000015ba6f980>",
  #   "rack.tempfiles": [

  #   ]
  # }
  binding.pry
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
end)
