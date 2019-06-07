

```ruby
# Gemfile

group :test do
  gem "capybara"
  gem "selenium-webdriver"
  gem "webdrivers"
end
```


```ruby
# spec/rails_helper.rb

require "selenium/webdriver"
require "webdrivers"

# Capybara.asset_host = "http://localhost:3000"

Capybara.register_driver :chrome do |app|
  Capybara::Selenium::Driver.new(app, browser: :chrome)
end

Capybara.register_driver :headless_chrome do |app|
  capabilities = Selenium::WebDriver::Remote::Capabilities.chrome(
    chromeOptions: { args: %w[headless disable-gpu] }
  )

  Capybara::Selenium::Driver.new app,
    browser: :chrome,
    desired_capabilities: capabilities
end

Capybara.javascript_driver = :chrome
```

