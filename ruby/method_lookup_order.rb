
module A
  def hello
    "hello from A"
  end
end

module B
  def hello
    "hello from B"
  end
end

module C
  include A
  # def hello
  #   "hello from C"
  # end
end


class Bar
  def hello
    "hello from Bar"
  end
end

class Foo < Bar
  include B
  include C

  # def hello
  #   "hello from Foo"
  # end
end


f = Foo.new

# Question: if #hello is defined in A and B, which one will be found first?
# Answer: A. The method lookup proceeds from bottom to top and searches all of C and anything it includes before moving on to search B
p f.hello
