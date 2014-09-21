puts "in filey"
# require './bar/filey2.rb'

# Ruby idiom for getting the path to a file in the same dir as current one
# Because __FILE__ includes the filename we use the ../ to get rid of it
puts File.expand_path('../besideme.rb', __FILE__)
puts File.expand_path('besideme.rb', File.dirname(__FILE__))

Dir.chdir '../../'
puts __FILE__
