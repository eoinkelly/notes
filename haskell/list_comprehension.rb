
# A list comprehension in Ruby
filtered_list = (1..10).map { |x| (x % 2 == 0) ? nil : x }.compact.map { |x| x * 2 }

puts filtered_list 
