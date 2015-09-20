# Configure SSL HTTP client in Ruby

## Sources

* [Good blog](http://www.braintreepayments.com/blog/sslsocket-verify_mode-doesnt-verify/)

How ruby verifies server identity

* `Net::HTTP.new(url, port).http.ca_file` is
    * a path to a local file
    * file is the "CA certification file"
    * file can contain several certificates in PEM format

    Net::HTTP.verify_mode[RW]
        Sets the flags for server the certification verification at beginning of SSL/TLS session.
        OpenSSL::SSL::VERIFY_NONE or OpenSSL::SSL::VERIFY_PEER are acceptable.

## Option 1: Net::HTTP way

```ruby
require 'net/http'
require 'net/https'
require 'uri'

url = URI.parse "https://provider.example.com/path"
http = Net::HTTP.new(url.host, url.port)
http.use_ssl = true
http.ca_file = CA_FILE
http.verify_mode = OpenSSL::SSL::VERIFY_PEER
request = Net::HTTP::Get.new(url.path)
response = http.request(request)
```

# Option 2: SSL Socket way

Note the callback defined to verify the peer

```ruby
def verify_ssl_certificate(preverify_ok, ssl_context)
  if preverify_ok != true || ssl_context.error != 0
    err_msg = "SSL Verification failed -- Preverify: #{preverify_ok}, Error: #{ssl_context.error_string} (#{ssl_context.error})"
    raise OpenSSL::SSL::SSLError.new(err_msg)
  end
  true
end

socket = TCPSocket.new('some.server.com', 443)

ssl_context = OpenSSL::SSL::SSLContext.new()
ssl_context.verify_mode = OpenSSL::SSL::VERIFY_PEER

############
# NOTE: you have to manually implement this callback to verify a peer using SSLContext
############
ssl_context.verify_callback = proc do |preverify_ok, ssl_context|
  verify_ssl_certificate(preverify_ok, ssl_context)
end

ssl_context.ca_file = CA_FILE

ssl_socket = OpenSSL::SSL::SSLSocket.new(socket, ssl_context)
ssl_socket.sync_close = true
ssl_socket.connect

ssl_socket.puts("GET / HTTP/1.0")
ssl_socket.puts("")
while line = ssl_socket.gets
  puts line
end
```

