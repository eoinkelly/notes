# Matching not assignment
# #######################

# A = B does a pattern match between A and B, not assignment of B to A!

# you can used pattern matching of tuples to get back multiple return values
# from a function as a single tuple. The first atom in the tuple indicates
# success or fail which lets you know what to do with the other element in the
# tuple
# { :ok, _file } = File.open("/not/there.txt") # throws MatchError
{ :error, _file } = File.open("/not/there.txt") # works
