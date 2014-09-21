# I **think** this is only required on very old ruby
# require 'rubygems'

old_lp = $LOAD_PATH.dup

original_features = $LOADED_FEATURES.dup
puts "************"
puts "#{original_features.length} ORIGINAL LOADED FEATURES:"
puts original_features
puts "************"


# This is all you need to bring Bundler into your project - it will:
# 1. cause bundler to search for a Gemfile
# 2. **Prepend** the gems in there to $LOAD_PATH
#
# Note it will not actually require any features from those gems - the only
# things now added to $LOADED_FEATURES are those features that bundler itself
# requires
require 'bundler/setup'

puts '============'
puts 'LOAD PATH DELTA AFTER bundler/setup:'
puts $LOAD_PATH - old_lp
puts '============'

bundler_features = $LOADED_FEATURES - original_features
puts '************'
puts "#{bundler_features.length} FEATURES REQUIRED BY BUNDLER ITSELF:"
puts bundler_features
puts '************'

# At this point bundler has tweaked $LOAD_PATH but no features have yet been
# required

# Now you can either require the features you need manually or ask bundler to
# do it for you by calling `Bundler.require` and passing it the names of the
# groups in the Gemfile that you want to load features for.
# * :default is the name of the top-level Gemfile group
Bundler.require(:default, :eoin)

after_bundler_require_features = $LOADED_FEATURES - bundler_features - original_features
puts "************"
puts "#{after_bundler_require_features.length} FEATURES REQUIRED BY Bundle.require:"
puts after_bundler_require_features
puts "************"
