require 'pry'

class Blah
	private
	attr_accessor :var1

	public
	attr_accessor :var2
end

# Pretend this is in a file distant from the original defn of Blah
class Blah
	attr_accessor :var3
end

b = Blah.new
# b.var1 = 22
b.var2 = 33
b.var3 = 55

binding.pry
