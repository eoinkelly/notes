require "ruby-prof"

class Thing
  def initialize(a)
    @a = a
  end

  def do_thing
    puts "doing thing"
  end
end

RSpec.describe Thing do
  it "works" do

    RubyProf.start ###########################
    # ... code under profile ...
    t = Thing.new("aaaa")
    t.do_thing
    result = RubyProf.stop ###################

    # Print flat results to stdout as text
    printer = RubyProf::FlatPrinter.new(result)
    printer.print(STDOUT)

    # print output to file for Kcachegrind to display
    printer = RubyProf::CallTreePrinter.new(result)
    printer.print(path: ".", profile: "profile")
  end
end
