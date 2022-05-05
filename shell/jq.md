# jq

* expressions usually need to be single quoted to avoid your shell mangling them
* filters are either a map or a reduce
* can also construct objects as well as filtering them
* as well as filters it has a set of built-in operators and functions

```bash

# identity filter
. # unchanged output, just pretty printed and colored

# all filters are prefixed by .

# get first element from array
.[0] # get first element from top level array

# lets you get all the values from an array
# OR get all the values from an object
.[] # feed each element of the array one at a time into the pipeline

, # lets you feed the same input into two filters and outputs will be concatenated

# combine twi filters
| # lets you build up filters
.a.b.c
# same as
.a | .b | .c

# extract the name field from each thing and return an array of the values
.things[].name

# get first element of array
# then get the .commit field from that object
# then get the .message field from that object
jq '.[0] | .commit.message'

# exctract the commit message and build a new JSON object for it with newStuff as a key
jq '.[0] | { newStuff: .commit.message }'