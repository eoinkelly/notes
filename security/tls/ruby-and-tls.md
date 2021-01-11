## Ruby and TLS

* Ruby provides a wrapper for OpenSSL in standard lib
* can load it via `require openssl`
* Ruby OpenSSL library supports loading certificates from
    * a PEM format file containing CA certificates `http.ca_file`
    * a directory of PEM format files `http.ca_path`
    * an X509::CertificateStore
* Ruby compiles with C bindings for OpenSSL.


You can configure the verify mode by setting `Net::HTTP.verify_mode = OpenSSL::SSL::<some mode constant>`.
The valid mode constants (with their integer values shown in parens) are:

* OpenSSL::SSL::VERIFY_NONE (0)
* OpenSSL::SSL::VERIFY_PEER (1)
* OpenSSL::SSL::VERIFY_FAIL_IF_NO_PEER_CERT (2)
* OpenSSL::SSL::VERIFY_CLIENT_ONCE (4)


Ruby 1.9.2 and later will default to setting `OpenSSL::SSL::VERIFY_PEER`

I worked this out by comparing

    1.9.2: https://github.com/ruby/ruby/blob/ruby_1_9_2/lib/net/http.rb
    1.9.1: https://github.com/ruby/ruby/blob/ruby_1_9_1/lib/net/http.rb

and searching for `VERIFY_MODE` on the page which turns up the following line of docs in 1.9.2 but not 1.9.1

> If you set :use_ssl as true, you can use https and default value of #verify_mode is set as OpenSSL::SSL::VERIFY_PEER.

### Configuring a secure connection in Ruby

#### Option 1: Net::HTTP way

```ruby
require 'net/http'
require 'net/https'
require 'uri'

CA_FILE = "/path/to/certs.pem"

url = URI.parse "https://provider.example.com/path"
http = Net::HTTP.new(url.host, url.port)

http.use_ssl = true
http.ca_file = CA_FILE
http.verify_mode = OpenSSL::SSL::VERIFY_PEER

request = Net::HTTP::Get.new(url.path)
response = http.request(request)
```

#### Option 2: SSL Socket way

Note the gotcha that you must define a callback to verify the peer yourself!

```ruby
CA_FILE = "/path/to/certs.pem"

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

### Ruby default OpenSSL setup

```
# show default config file
$ ruby -ropenssl -e "puts OpenSSL::Config::DEFAULT_CONFIG_FILE"
/usr/local/etc/openssl/openssl.cnf

# Version of OpenSSL used by Ruby 2.2.3
$ ruby -ropenssl -e 'puts OpenSSL::OPENSSL_VERSION'
OpenSSL 1.0.2d 9 Jul 2015

# see what ruby openssl is using as defaults
$ ruby -ropenssl -e "puts OpenSSL::X509::DEFAULT_CERT_FILE"
/usr/local/etc/openssl/cert.pem

$ ruby -ropenssl -e "puts OpenSSL::X509::DEFAULT_CERT_DIR"
/usr/local/etc/openssl/certs
```

You can change these locations with

1. SSL_CERT_FILE and SSL_CERT_DIR environment variables.
2. the `ca_file` and `ca_path` properties of the Net::HTTP instance


OpenSSL environment variables

* `SSL_CERT_FILE`
    * A bundle of multiple PEM certificates in a single file, usually containing the CA bundle.
    * is sometimes a symlink to the real file

* `SSL_CERT_DIR`
    * empty on my system
    * OpenSSL expects to find each certificate in a file named by the
      certificate subject’s hashed name, plus a number extension that starts
      with 0. So if you inspect a non-empty SSL_CERT_DIR (on Ubuntu, for
      example) you will see a bunch of files named {HASH}.0, where HASH is a
      short hex string.
    * OpenSSL ships with a utility called c_rehash which you can invoke on a
      directory to have all certificates indexed with appropriately named
      symlinks. If you have multiple OpenSSL versions installed (on OS X, you
      likely will), beware: the hashing algorithm changed between OpenSSL 0.9.8
      and 1.0.1, so you’d want to use c_rehash distributed by the version which
      is actually going to use those certificates.

QUESTION: does ruby versions come with an openssl built-in or does it matter which openssl is on your system when ruby is built?

Ruby core team is discussion putting OpenSSL in a gem https://bugs.ruby-lang.org/issues/9612
