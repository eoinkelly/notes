require 'rubygems'
require 'selenium-webdriver'
require 'pry'

driver = Selenium::WebDriver.for :firefox # this opens browser
# driver.class = Selenium::WebDriver::Driver

driver.get "http://google.com" # returns empty string

element = driver.find_element :name => "q"
# element.class = Selenium::WebDriver::Element
element.send_keys "Cheese!"
element.submit

puts "Page title is #{driver.title}"

wait = Selenium::WebDriver::Wait.new(:timeout => 10)
# wait.class = Selenium::WebDriver::Wait

wait.until { driver.title.downcase.start_with? "cheese!" }

puts "Page title is #{driver.title}"

# if driver.quit is not called then the browser stays open after this script
# exits
# driver.quit
