# arrays - oh my!
## a.k.a. stuff Eoin never knew existed but probably should have ...

## Creating arrays


## getting things out

## clever []
```ruby

# slicing
# my_array[start, slice_size]

a = [10, 11, 12, 13, 14]
start = 1
slice_size = 3
a[start, slice_size] # => [11, 12, 13]

# slicing in action
n = 3
a[-n,n] # gets the last n elements from collection
a[0,n] # gets the first n elements from collection

# of course this also works (and is probably nicer)
a.first(n)
a.last(n)

# make a stack (LIFO)

stack = []
stack.push "a"
stack.push "b"
stack.push "c"
stack # => ["a", "b", "c"]
stack.pop   # => "c"
stack.pop   # => "b"

? use case for stack ?

# make a FIFO queue
queue = []
a.push "foo"
a.push "bar" 
a.shift       # => "foo" 
a.unshift     # => "bar"

? use case for FIFO ?

# using ranges
a = ["a", "b", "c", "d"]
a[0..2] # => ["a", "b", "c"]

# get everything except first and last elements
middle = a[1..-2]

# handy for recursion 

args = [1, 2, 3] 

# split the array into a head and a tail
first = a[0]
rest = a[1..-1]

first  # => 1 
rest   # => [2, 3]

# or even nicer ...

first, *rest = args # this is called destructuring btw

first               # => 1 
rest                # => [2, 3]

# ? demo a recursion algorithm here

# replacing a chunk of an array at once 
a[0..2] = ["a", "b", "c"]

```

### quick mass assignment
```ruby
array = ["bird", "is", "the", "word"]
a, b, c = array
a # => "bird"
b # => "is"
c # => "the"
```

## converting to other things

### Converting to string
```ruby
a = ['hello', 'there']
a.join('-') # => 'hello-there'
a * '-'     # => 'hello-there'
```

```ruby
creating arrays
[*items] converts a single object into an array with that single object
of converts an array back into, well, an array again
[*items].each do |item|
  # …end
```

Three ways to garuantee that a variable is an array

```ruby
Array(my_array)
my_array.present? ? my_array : []
my_array || []
```

# Strings

## formatting interpolation

```ruby
foo = "bird"
bar = "wat?"
"%s is the word" % foo            # => "bird is the word"
"%s is the word %s" % [foo, bar]  # => "bird is the word wat?"
```

## rails #blank? and #present?

Instead of 

```ruby
full_name = current_user.full_name

if ! my_str.blank?
  # do stuff
end
```

try

```ruby
full_name = current_user.full_name

if my_str.present?
  # do stuff
end
```
