require "httparty"

##
#
class TestParty
  include HTTParty
  base_uri "https://my.api"

  def initialize
    @options = {}

    # explicitly disable server cert verification (terrible idea in production)
    # @options = { verify: false }
  end

  def test_url
    self.class.get("/", @options)
  end
end

begin
  puts TestParty.new.test_url
rescue OpenSSL::SSL::SSLError => e
  if e.message =~ /certificate verify failed/
    puts "The server cert failed verifcation with: #{e}"
  else
    raise e # unknown failure so let it bubble up
  end
else
  puts "Ruby thinks the server certificate is fine."
end
