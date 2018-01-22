# run this file:
# $ chef-apply simple.rb

# cleans up what simple.rb creates

file "hello.txt" do
  action :delete
end

file "#{ENV['HOME']}/temp.txt" do
  action :delete
end
