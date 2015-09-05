# The gem method will check that rack at the given version is installed and
# will add it to your $LOAD_PATH

old_load_path = $LOAD_PATH.dup

gem 'bundler', '1.10.6'

# notice that the bundler gem /lib has been added to LOAD_PATH
p $LOAD_PATH - old_load_path

# gem is used as a workaround for require not understanding gems
require 'bundler'
