class CucumberGreeter
  def greet
    "Hello Cucumber!"
  end
end

# these guys are the executable code that cucumber magically finds based on it's parsing of the .feature file
# The format of the scenario in the .feature file is roughly:
# Scenario: {scenario name}
#   {function name} {some text to match against a regexp}

# so cucumber does some regexp matching to find what block to invoke in this file
# this file is just a collection of blocks associated with lines from the scenario in the .feature file

# You can't arbitrarily create new functions e.g. this doesn't work
# Eoin /^is awesome$/ do
#   puts "eoin rocks"
# end

# so Given, When, Then are methods defined in cucumber

Given /^a greeter$/ do
  @greeter = CucumberGreeter.new
end

When /^I send it the greet message$/ do
  @message = @greeter.greet
end

Then /^I should see "([^"]*)"$/ do |greeting|
  @message.should == greeting
end

