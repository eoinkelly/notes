require './toy'
require './middlewares/crappy'
require './middlewares/shitty'

# Rack::Directory works fine as the terminal app
run Rack::Directory.new(__dir__)


use(Rack::Directory, __dir__)
# my crappy middlewares
# use Crappy
# use Shitty

run Toy
