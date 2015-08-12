# require 'rack'

##
#
class Toy
  def self.call(_env)
    puts 'Toy: got the request'
    # request = Rack::Request.new(env)

    # do stuff to make the request

    puts 'Toy: sending response'
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
