# run this file:
# $ chef-apply simple.rb

require "pry"

require "pp"
pp node.debug_value("ipaddress")
# => [["default", :not_present],
#  ["env_default", :not_present],
#  ["role_default", :not_present],
#  ["force_default", :not_present],
#  ["normal", :not_present],
#  ["override", :not_present],
#  ["role_override", :not_present],
#  ["env_override", :not_present],
#  ["force_override", :not_present],
#  ["automatic", "192.168.1.69"]]
# creates a new file in your cwd

file "hello.txt" do
  content "Hello world"
end

file "#{ENV['HOME']}/temp.txt" do
  content "Hello world"
end
