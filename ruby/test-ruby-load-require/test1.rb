puts 'hi from test1'
puts '------------------'
puts 'load path is'
$LOAD_PATH.each { |p| puts p }
puts '------------------'

puts '------------------'
puts 'loaded features:'
$LOADED_FEATURES.each { |p| puts p }
puts '------------------'

require 'zlib'

puts '------------------'
puts 'loaded features:'
$LOADED_FEATURES.each { |p| puts p }
puts '------------------'
test1_loc1 = 23

puts local_variables

load 'bar/b1.rb', true # works
# load "./bar/b1.rb" # works
# load "./bar/b1" # breaks
# load "bar/b1" # breaks

puts "global from bar: #{$bar_global}"

# require "bar/b1" # breaks
require './bar/b1' # works
