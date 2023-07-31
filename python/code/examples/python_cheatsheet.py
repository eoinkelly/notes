# a snippet which does similar to binding.pry in ruby
# https://gist.github.com/obfusk/208597ccc64bf9b436ed
import code

code.interact(local=dict(globals(), **locals()))

# http://book.pythontips.com/en/latest/object_introspection.html
x = "an example object, some object, any object"

dir(x)  # show attributes and methods of an object
type(x)  # show type
id(x)  # show id
help(x)  # show docs about x

import inspect

inspect.getmembers(x)
help(inspect)  # see help about the inspect module
inspect.getsourcefile(inspect)  # return path to source file of inspect (modules only)
inspect.getsource(
    inspect
)  # return source code of module 'inspect' as string (modules only)

# Debugger
# https://docs.python.org/3/library/pdb.html
import pdb

pdb.set_trace()  # stops execution on this line
