require "net/http"
require "net/https"
require "openssl"
require "uri"

# Dump info about OpenSSL on your machine
# puts "OpenSSL default config: " + OpenSSL::Config::DEFAULT_CONFIG_FILE
# puts "OpenSSL version: " + OpenSSL::OPENSSL_VERSION
# puts "OpenSSL default cert file: " + OpenSSL::X509::DEFAULT_CERT_FILE
# puts "OpenSSL default cert dir: " + OpenSSL::X509::DEFAULT_CERT_DIR

# Enable debugging mode on OpenSSL
# OpenSSL.debug = true

url = URI.parse "https://my.api/endpoint"
http = Net::HTTP.new(url.host, url.port)

# this makes Net::HTTP dump debugging output (similar to curl -v). It dumps
# output at the HTTP layer but doesn't show much about the SSL layer
http.set_debug_output $stdout
http.use_ssl = true

# set the default certificate authority file if your installation of OpenSSL
# requires it
# CA_FILE = "/usr/local/etc/openssl/cert.pem"
# http.ca_file = CA_FILE

# explicitly set the verify mode
# http.verify_mode = OpenSSL::SSL::VERIFY_PEER

request = Net::HTTP::Get.new(url.path)
response = http.request(request)

puts response.body # or response.to_yaml if you prefer
