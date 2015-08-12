##
#
class Crappy
  def initialize(app)
    @app = app
  end

  def call(env)
    # env represents the request - mutate it here
    puts 'Crappy: improving request'

    # call the next thing in the chain
    status, headers, body = @app.call(env)

    # do stuff with the response
    puts 'Crappy: improving the response'

    # return the mutated response to the previous bit of hte chain
    [status, headers, body]
  end
end
