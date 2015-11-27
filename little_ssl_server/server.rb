require "sinatra/base"
require "logger"

##
#
class Application < Sinatra::Base
  configure do
    set :bind, "0.0.0.0"
    set :port, 443
    set :logging, Logger::DEBUG
  end

  get "/" do
    "Hello, SSL\n"
  end
  get "/health" do
    "Hello, health checker\n"
  end

  post "/entries" do
    "fake entries\n"
  end

  def self.run!
    super do |server|
      server.ssl = true
      server.ssl_options = {
        cert_chain_file: File.dirname(__FILE__) + "/domain.crt",
        private_key_file: File.dirname(__FILE__) + "/domain.key",
        verify_peer: false
      }
    end
  end

  run!
end
