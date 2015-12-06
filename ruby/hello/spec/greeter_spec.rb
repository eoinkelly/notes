class RSpecGreeter
  def greet
    "Hello RSpec!"
  end
end

describe "RSpec Greeter" do # declare an example group
  it "should say 'Hello RSpec!' when it receives the greet() message" do # declare an example

    # our examples reads like "Given an RSpecGreeter object, when we call the greet method the output should be 'Hello RSpec!'"

    # setup the context aka our "given"
    greeter = RSpecGreeter.new

    # this is the "when" aka the action we care about
    greeting = greeter.greet

    # this is the "then" aka the expected outcome
    greeting.should == "Hello RSpec!"
  end
end

# so each example group contains one or more examples
# and each example has a "given X when Y then Z" structure