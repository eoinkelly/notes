require './toy'
require './middlewares/crappy'
require './middlewares/shitty'

use Crappy
use Shitty

run Toy
