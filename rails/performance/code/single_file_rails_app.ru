##
# Usage:
#
#     # In terminal #1:
#     $ gem install puma
#     $ puma ./single_file_rails_app.ru
#
#     # In terminal #2:
#     $ curl http://localhost:9292/
#
require "rails"
require "action_controller"


class SimpleApp < Rails::Application
  routes.append { root "simple#show" }
  config.eager_load = true
  config.cache_classes = true
  config.logger = Logger.new(STDOUT)
  config.log_level = :error
  config.secret_key_base = "some long string of stuff"
end

class SimpleController < ActionController::Base
  def show
    render plain: "Hello there"
    # OR
    # render json: { some_object: "some_value" }
  end
end

# Normally in config/environment.rb
SimpleApp.initialize!

# Add rack middlewares to taste:
# use SomeMiddleWare
# use SomeotherMiddleWare

# Normally in ./config.ru
run SimpleApp
