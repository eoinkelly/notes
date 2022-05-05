# Ruby argument syntax examples

def ex_1(*positionals, **keywordys)
	# you have to construct separate collections of the keyword and positional args
	puts "Positionals:" +  positionals.inspect
	puts "Keywords:" + keywordys.inspect
end


ex_1(:a, "h", foo: "blah", bar: "blas")
# output:
# Positionals:[:a, "h"]
# Keywords:{:foo=>"blah", :bar=>"blas"}

# ex_1(:a, "h", foo: "blah", bar: "blas", :yes, blue: "there") # fails, you cannot mix positionals and keywords like this


def ex_2(a, b, c, d:, e:)
	puts "Args:" + [a, b, c, d, e].inspect
end

ex_2(1, 44, 56, d: 66, e: 77)
# Args:[1, 44, 56, 66, 77]

def ex_3(a, b, c, **rest)
	puts "Args:" + [a, b, c, rest].inspect
end

ex_3(1, 44, 56, d: 66, e: 77)
# Args:[1, 44, 56, {:d=>66, :e=>77}]

def ex_4(*stuff, **rest)
	puts "Args:" + [stuff, rest].inspect
end

ex_4(1, 44, 56, d: 66, e: 77)
# Args:[[1, 44, 56], {:d=>66, :e=>77}]