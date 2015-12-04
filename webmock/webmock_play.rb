require "webmock"
include WebMock::API
require "net/http"

require "pry"

# https://github.com/bblimke/webmock

uri = "api.raygun.io"

# form is: stub_request(method, url)
stub_request(:any, uri).to_return(status: [500, "Internal Server Error"])
# stub_request(:any, uri).to_timeout

req = Net::HTTP::Get.new("/")
response = Net::HTTP.start(uri) { |http| http.request(req) }

p response.code
p response.message
