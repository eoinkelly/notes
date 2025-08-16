# Coming to Python from Ruby

A quick reference of changes a Rubyist would see working in Python.

## Open questions

How quickly are python apps typically upgraded to newer versions of python?
Similar to ruby or more like Java? Python has a lot more features for exceptions
as flow control - are they faster in python than ruby to make this ok?

## Sources

- http://learnxinyminutes.com/docs/python/
- Python in a Nutshell

## General

- Python Like ruby:
    - default file encoding is UTF-8
    - has magic comments at start of file to set encoding e.g.
      `# coding: iso-8859-1`
- Variable naming rules seem similar to Ruby
    - uppercase starts class (convention only, not a rule)
    - identifiers are snake_case
    - `_` prefix is a convention signalling private
    - `__` prefix is a convention signalling **strongly private**
    - `_` identifier in repl holds the result of the last command
    - `__` prefix **and** suffix (e.g. `__thing__`) means the identifier is a
      language defined special name
- difference from Ruby: assignment is not an expression by default
    - You can use `:=` instead of `=` to make assignment an expression

## Numbers

- number literals are the same, `_` separators, `0o`, `0x` `0b` prefixes
- Difference from Ruby: complex number syntax
    - python has syntax for complex literals, ruby does not
    - python calls the imaginary part `j`, Ruby calls it `i` :shrug:
    - python
        ```python
        x = 3+5j
        x.conjugate() # 3-5j
        ```
    - ruby
        ```ruby
        x = Complex(3, 5) # => (3+5i)
        ```

```python
# number methods
float.from_hex()
float.hex()
float.is_integer()
```

## Iteration

- Python has an similar concept of Ruby Enumerable called Iterable
- A collection is iterable if it implements `__iter__()` which produces the next
  object in the sequence

## Lambdas and callables

- You can make lambdas with `lambda:`
- functions are first-class objects in Python

## Classes and objects

- python can implement ruby' `method_missing` by overriding both of
  `__getattr__` and `__setattr__`
    - `__getattr__` is what python calls on an object to get its attribute

## Strings

- single and double quote literals are identical - they both expand `\` special
  characters
    - the only reason to choose one or the other is if you string already
      contains quote characters
    - you can get Ruby's single quote behaviour by creating a "raw string" e.g.
      `a = r"special\nchars\tignored"`
    - example:
        ```python
        >>> a = "hello\nthere"
        >>> b = 'hello\nthere'
        >>> a == b
        True
        >>> print(a)
        hello
        there
        >>> print(b)
        hello
        there
        ```
    - style guides usually have an opinion on which quote to use
- strings are always immutable in python unlike ruby.
- triple quoted strings (`"""` or `'''`) line breaks in the literal remain as
  newlines in the resulting string object
    - they are equivalent to Ruby `<<-`
    - Python does not have an equivalent of `<<~` :sad_face:
- Python has multiple related string types unlike Ruby which has one. Python
  has:
    - `str` type is a sequence of UTF-8 characters
    - `bytes` immutable type is a sequence of arbitrary bytes
    - `bytearray` mutable version of `bytes`

```python
# creating bytes type

# option 1:
>>> a = b'i am of type bytes'
>>> type(a)
<class 'bytes'>

# option 2: # bytes() creates an immutable array of bytes
>>> b = bytes([99, 45, 65])
>>> type(b)
<class 'bytes'>

# option 3: bytearray() creates a mutable array of bytes
>>> ba = bytearray([97, 98, 99])
>>> ba[1] = 97
>>> print(ba.decode())
aac
>>> type(ba)
<class 'bytearray'>

# you can combine raw and bytes prefixes
raw_bytes = rb'i am bytes but \n does not become newline'

# changing between string types
my_str = my_bytes.decode
my_bytes_2 = my_str_2.encode
```

## Objects

- Python objects
    - are open like OpenStruct in ruby
    - differences
        - Python lets you grab "method objects" from classes and use them
          anywhere python expects a "callable"

## Types

- Use as barewords in syntax
- Python types are themselves of class `type`

    ```python
    >>> xx = type("hi")
    >>> xx
    <class 'str'>

    # types have class 'type'
    >>> type(xx)
    <class 'type'>

    # types have class 'type'
    >>> yy = type(xx)
    >>> yy
    <class 'type'>
    >>> type(yy)
    <class 'type'>
    ```

- Python types:
    ```
    bool
    str
    int
    float
    bytes
    (others...)
    ```
- Useful type functions
    ```python
    type(thing)
    instanceof(thing, type)
    instanceof(thing, [type,type2,type3])
    instanceof(thing, type|type2|type3)
    ```

## None is the nil/none

- type of `None` is `NoneType`
- is equal to itself
    ```py
    >>> type(None)
    <class 'NoneType'>
    >>> None == None
    True
    ```

## Booleans

- `bool` type has two values: `True`, `False` (case matters - true and false are
  not aliases)
- Python has 11 falsy values - use `bool(my_val)` to test

    ```python
    bool([]) # empty list
    bool(()) # empty tuple
    bool({}) # empty dict
    bool(set()) # empty set
    bool("") # empty string
    bool(range(0)) # empty range

    # any number 0
    bool(0) # int 0
    bool(0.0) # float 0
    bool(0j) # complex 0

    # constants
    bool(None)
    bool(False)
    ```

- this is very different to Ruby - only `nil` and `false` are falsy in Ruby

Comparison

```python
# boolean comparison operators
<
<=
==
>=
>

# logical operators
and # returns RHS operand if you don't pass it bools
or
not
^ # xor


# bitwise operators
& # bitwise AND
| # bitwise OR
^ # bitwise XOR
~ # bitwise NOT
<< # bitwise shift left:  bits << num_places
>> # arithmetic bitwise shift right: bits >> num_places
# python does not have a logical/unsigned shift right


5 / 2 # division operator
5 // 2 # floor division operator, returns the largest in that is less than or equal to the float result


# identity operators
a is b # True if a and b are references to the same object
a is not b # Negated of above

# python id(my_ob) is equivalent to Ruby #object_id
a = {}
b = {}
id(a)
# 4378891648
id(b)
# 4378892032
a is b
# False
c = a
id(c)
# 4378891648
a is c
True
```

## Useful functions to introspect objects

```python
dir(my_ob) # shows you all the "attributes" in an object

isinstance(my_ob, my_type)
isinstance(my_ob, (my_type_1, my_type_2)) # can pass a tuple of types to match any
isinstance(my_ob, my_type_1 | my_type_2) # can use | syntax since 3.10

id(my_ob) # shows you internal id
```

## Differences

Python will Unicode NFKC normalise identifiers while parsing so don't use
characters which would normalise to something else e.g.

```python
a, o = 100, 101
ª, º = 200, 201 # these are normalised to a, o at parse time. They are  "homoglyphs" of a, o
print(a, o, ª, º) # => 200 201 200
201
```

Ruby doesn't do this.

```ruby
a, o = 100, 101
ª, º = 200, 201
p [a, o, ª, º] # => [100, 101, 200, 201]
a == ª # => false
o == º # => false
```

## Object oriented

- Python is not strictly OO
- Classes are callable objects. You invoke them like a function and they run
  their internal `__init__` method as part of initialization.
- You are encouraged to name classes with initial cap but not required
- Each class in python has a "meta class" which defines how a class instance
  should be created.
    - The default metaclass is `type` but you can customise it if you need fine
      grained control over how classes are created

```python
class SomeClass(metaClass=MyClass):
	pass
```

### type()

- a built-in function
- can tell you the type of an object
- can also be used to create new type object dynamically by calling it with 3
  args (see example below)

```python
# this creates a class at compile time
class TypeExample:
    x: int = 42

# this creates an equivalent class at runtime
TypeExampleNoSugar = type('TypeExampleNoSugar', (), {'x': 42})

my_c = TypeExample()
print(my_c.x)  # 42
my_object = TypeExampleNoSugar()
print(my_object.x)  # 42
```

## modules

- each python source file is a module
- two ways of pulling in a dependency from another module to your current
  module:
    1. `import` statement
        - `import foo`
            - looks for `foo.py` in ??
            - binds the _module object_ to a variable named `foo` in current
              scope
        - optional aliasing import foo as bar
        - imports **all** the identifiers defined in `foo.py` but puts them
          nicely in their own namespace
        - apparently import is better style than from in most cases
        - examples
            ```python
            import foo
            import foo as bar
            import a, b, c
            import a as aa, b as bb, c as cc
            ```
    2. `from` statement
        - TODO
        - lets you choose which attributes of the module you want to import into
          your namespace
        - example
            ```python
            from blan import a, b as bb, c, d as dd
            from blah import * # not quite the same as 'import blah'
            ```
        - `from blah import *` imports the attributes whosenames appear in the
          `__all__` attribute value (a list)
        - you can specify an `__all__` but if you don't then all attribues
          **except thosw with an \_ prefix** are imported
        - basically this is a more privacy respecting version of `import blah`
        - it also doesn't put `blah` itself in the namespace of our module so
          modules which import us don't get access to `blah`
        - but it also doesn't namespace the identifiers which are imported so
          you have to be careful not to override
- `.` separators in a module name indicate stepping into a package
- Modules can also be written in a compiled lang
- A module
    - is an object
    - has arbitrarily named attributes which you can bind and reference
- modules are first-class objects (can put in containers, pass as args etc.)
- `sys.modules`
    - a dictionary of type `str: <class 'module'>`
    - holds all the modules already loaded by python in your session as
      `module_name: module_object`

        ```python
        import sys

        type(sys.modules)
        # <class 'dict'>

        dict.keys(sys.modules)
        # dict.keys(
        #     [
        #         "sys",
        #         "builtins",
        #         "_frozen_importlib",
        #         "_imp",
        #         "_thread",
        #         "_warnings",
        #         "_weakref",
        #         "_io",
        #         "marshal",
        #         "posix",
        #         "_frozen_importlib_external",
        #         "time",
        #         "zipimport",
        #         "_codecs",
        #         "codecs",
        #         "encodings.aliases",
        #         "encodings",
        #         "encodings.utf_8",
        #         "_signal",
        #         "_abc",
        #         "abc",
        #         "io",
        #         "__main__",
        #         "_stat",
        #         "stat",
        #         "_collections_abc",
        #         "genericpath",
        #         "posixpath",
        #         "os.path",
        #         "os",
        #         "_sitebuiltins",
        #         "_distutils_hack",
        #         "types",
        #         "importlib._bootstrap",
        #         "importlib._bootstrap_external",
        #         "warnings",
        #         "importlib",
        #         "importlib._abc",
        #         "itertools",
        #         "keyword",
        #         "_operator",
        #         "operator",
        #         "reprlib",
        #         "_collections",
        #         "collections",
        #         "_functools",
        #         "functools",
        #         "contextlib",
        #         "importlib.util",
        #         "importlib.machinery",
        #         "mpl_toolkits",
        #         "site",
        #         "readline",
        #         "atexit",
        #         "_ast",
        #         "enum",
        #         "ast",
        #         "_opcode",
        #         "opcode",
        #         "dis",
        #         "collections.abc",
        #         "_sre",
        #         "re._constants",
        #         "re._parser",
        #         "re._casefix",
        #         "re._compiler",
        #         "copyreg",
        #         "re",
        #         "token",
        #         "tokenize",
        #         "linecache",
        #         "inspect",
        #         "rlcompleter",
        #     ]
        # )
        ```

- module body is a sequence of statements
    - executes immediately the first time something imports it
    - any identifiers it creates become attributes of its module object in
      `sys.modules`
    - you can put side-effects in top level of your module but shouldn't in most
      cases
- Q: How do you make something private in a module?
    - A: You can't but you can signal it by naming with `_` prefix
- Python does not enforce any privacy at the module level
    - The `_` naming prefix is a convention but python itself makes everything
      available
    - You can access modules imported by a module you import.

        ```python
        # foo.py
        foo_flag = "hello from foo"
        _private_foo_flag = "hello from foo"

        # bar.py
        import foo
        bar_flag = "hi from bar"

        # baz.py
        import bar
        print(bar.bar_flag) # works
        print(bar.foo.foo_flag) # works
        print(bar.foo._private_foo_flag)
        ```

* modules have `__get_attr__(name)` dunder method which acts like a
  method_missing at the module level
    - you can use it to lazily create attrs when they are first called
* you can add things to the module object from outside it but you shouldn't
* modules contain
    - `__dict__` the dictionary which holds all the attributes of the model.
        - `mymod.foo` is eqivalent to `mymod.__dict__["foo"]` except you get
          different exceptions in error cases (`attributeerror` vs `keyerror`)

            ```python
            import baz

            >>> dict.keys(baz.__dict__)
            dict_keys(['__name__', '__doc__', '__package__', '__loader__', '__spec__', '__file__', '__cached__', '__builtins__', 'bar'])

            # built-ins for a trivial imported module
            >>> dict.keys(baz.__builtins__)
            dict_keys(['__name__', '__doc__', '__package__', '__loader__', '__spec__', '__build_class__', '__import__', 'abs', 'all', 'any', 'ascii', 'bin', 'breakpoint', 'callable', 'chr', 'compile', 'delattr', 'dir', 'divmod', 'eval', 'exec', 'format', 'getattr', 'globals', 'hasattr', 'hash', 'hex', 'id', 'input', 'isinstance', 'issubclass', 'iter', 'aiter', 'len', 'locals', 'max', 'min', 'next', 'anext', 'oct', 'ord', 'pow', 'print', 'repr', 'round', 'setattr', 'sorted', 'sum', 'vars', 'None', 'Ellipsis', 'NotImplemented', 'False', 'True', 'bool', 'memoryview', 'bytearray', 'bytes', 'classmethod', 'complex', 'dict', 'enumerate', 'filter', 'float', 'frozenset', 'property', 'int', 'list', 'map', 'object', 'range', 'reversed', 'set', 'slice', 'staticmethod', 'str', 'super', 'tuple', 'type', 'zip', '__debug__', 'BaseException', 'BaseExceptionGroup', 'Exception', 'GeneratorExit', 'KeyboardInterrupt', 'SystemExit', 'ArithmeticError', 'AssertionError', 'AttributeError', 'BufferError', 'EOFError', 'ImportError', 'LookupError', 'MemoryError', 'NameError', 'OSError', 'ReferenceError', 'RuntimeError', 'StopAsyncIteration', 'StopIteration', 'SyntaxError', 'SystemError', 'TypeError', 'ValueError', 'Warning', 'FloatingPointError', 'OverflowError', 'ZeroDivisionError', 'BytesWarning', 'DeprecationWarning', 'EncodingWarning', 'FutureWarning', 'ImportWarning', 'PendingDeprecationWarning', 'ResourceWarning', 'RuntimeWarning', 'SyntaxWarning', 'UnicodeWarning', 'UserWarning', 'BlockingIOError', 'ChildProcessError', 'ConnectionError', 'FileExistsError', 'FileNotFoundError', 'InterruptedError', 'IsADirectoryError', 'NotADirectoryError', 'PermissionError', 'ProcessLookupError', 'TimeoutError', 'IndentationError', 'IndexError', 'KeyError', 'ModuleNotFoundError', 'NotImplementedError', 'RecursionError', 'UnboundLocalError', 'UnicodeError', 'BrokenPipeError', 'ConnectionAbortedError', 'ConnectionRefusedError', 'ConnectionResetError', 'TabError', 'UnicodeDecodeError', 'UnicodeEncodeError', 'UnicodeTranslateError', 'ExceptionGroup', 'EnvironmentError', 'IOError', 'open', 'quit', 'exit', 'copyright', 'credits', 'license', 'help', '_'])


            # built-ins for the main module in repl
            >>> dict.keys(__builtins__.__dict__)
            dict_keys(['__name__', '__doc__', '__package__', '__loader__', '__spec__', '__build_class__', '__import__', 'abs', 'all', 'any', 'ascii', 'bin', 'breakpoint', 'callable', 'chr', 'compile', 'delattr', 'dir', 'divmod', 'eval', 'exec', 'format', 'getattr', 'globals', 'hasattr', 'hash', 'hex', 'id', 'input', 'isinstance', 'issubclass', 'iter', 'aiter', 'len', 'locals', 'max', 'min', 'next', 'anext', 'oct', 'ord', 'pow', 'print', 'repr', 'round', 'setattr', 'sorted', 'sum', 'vars', 'None', 'Ellipsis', 'NotImplemented', 'False', 'True', 'bool', 'memoryview', 'bytearray', 'bytes', 'classmethod', 'complex', 'dict', 'enumerate', 'filter', 'float', 'frozenset', 'property', 'int', 'list', 'map', 'object', 'range', 'reversed', 'set', 'slice', 'staticmethod', 'str', 'super', 'tuple', 'type', 'zip', '__debug__', 'BaseException', 'BaseExceptionGroup', 'Exception', 'GeneratorExit', 'KeyboardInterrupt', 'SystemExit', 'ArithmeticError', 'AssertionError', 'AttributeError', 'BufferError', 'EOFError', 'ImportError', 'LookupError', 'MemoryError', 'NameError', 'OSError', 'ReferenceError', 'RuntimeError', 'StopAsyncIteration', 'StopIteration', 'SyntaxError', 'SystemError', 'TypeError', 'ValueError', 'Warning', 'FloatingPointError', 'OverflowError', 'ZeroDivisionError', 'BytesWarning', 'DeprecationWarning', 'EncodingWarning', 'FutureWarning', 'ImportWarning', 'PendingDeprecationWarning', 'ResourceWarning', 'RuntimeWarning', 'SyntaxWarning', 'UnicodeWarning', 'UserWarning', 'BlockingIOError', 'ChildProcessError', 'ConnectionError', 'FileExistsError', 'FileNotFoundError', 'InterruptedError', 'IsADirectoryError', 'NotADirectoryError', 'PermissionError', 'ProcessLookupError', 'TimeoutError', 'IndentationError', 'IndexError', 'KeyError', 'ModuleNotFoundError', 'NotImplementedError', 'RecursionError', 'UnboundLocalError', 'UnicodeError', 'BrokenPipeError', 'ConnectionAbortedError', 'ConnectionRefusedError', 'ConnectionResetError', 'TabError', 'UnicodeDecodeError', 'UnicodeEncodeError', 'UnicodeTranslateError', 'ExceptionGroup', 'EnvironmentError', 'IOError', 'open', 'quit', 'exit', 'copyright', 'credits', 'license', 'help', '_'])
            ```

    - `__name__`
    - `__file__`
        - built-in modules like `sys` do not have this

* packages have the following dunder attrs
    - `__path__`
* python is quite open to metaprogramming - you can `import builtins` and
  override those methods for your module.
* if the first statement in a file is a string it becomes `__doc__` in the
  module.
    - `print(mymod.__doc__)` will show the docs
* you can handle import errors by catching the `importerror` exception
    ```python
    try:
        import foo
    except importerror:
        # do something else
        foo = none
    ```
* `import` and `importlib.import_module` both use built-in `__import__` function
  under the hood
    - we shouldn't use `__import__` directly but understanding it is good
* `__import__` importing a module `m`
    - steps
        1. check if module is builtin by looking in `sys.builtin_module_names`.
           If it finds it, call the modules initialization function
            ```python
            sys.builtin_module_names
            (
                '_abc',
                '_ast',
                '_codecs',
                '_collections',
                '_functools',
                '_imp',
                '_io',
                '_locale',
                '_operator',
                '_signal',
                '_sre',
                '_stat',
                '_string',
                '_symtable',
                '_thread',
                '_tokenize',
                '_tracemalloc',
                '_warnings',
                '_weakref',
                'atexit',
                'builtins',
                'errno',
                'faulthandler',
                'gc',
                'itertools',
                'marshal',
                'posix',
                'pwd',
                'sys',
                'time',
                'xxsubtype'
            )
            ```
        1. Check `sys.modules["M"]` - return the value there if it exists
        1. Otherwise create a new module object with `__name__` of `M` and store
           it in `sys.modules["M"]`
        1. Search the filesystem for the corresponding module file
        1. Execute the file statements using `sys.modules["M"]` as context
        1. Compile the `.py` file to `M.<tag>.pyc` unless the bytecode file is
           already present and is newer than the `.py` file.
        1. Now it has the bytecode, execute it, starting with the module's
           initialization function `PyInit_<module_name>`
    - notice that module evaluation happens only once
* `importlib.reload("M")` to force reload a module (handy in the repl) _ it does
  not re-bind existing variables i.e. has no effect on existing references to
  old attributes _ basically it works better with `import foo` rather than
  `from foo import ...` because import only creates one reference which reload
  will rebind. \* It is not recursive
* Absolute imports
    - python finds modules by searching `sys.path`
        ```python
        >>> sys.path
        [
            '', # cwd of the main program file
            '/Users/eoinkelly/.pyenv/versions/3.11.2/lib/python311.zip',
            '/Users/eoinkelly/.pyenv/versions/3.11.2/lib/python3.11',
            '/Users/eoinkelly/.pyenv/versions/3.11.2/lib/python3.11/lib-dynload',
            '/Users/eoinkelly/.pyenv/versions/3.11.2/lib/python3.11/site-packages'
        ]
        ```
    - the entries are searched **in order**
    - `PYTHONPATH` is used to initialize `sys.path`
* Relative imports
    - only available in `from ...` not in `import ...`
        ```python
        from . import bar # import bar module from current package, doesn't work at top level but works within packages
        ```
* items on sys.path can be
    - A directory name
    - path to a zip file
* python uses the module name to resolve the file name. It tries the following
  extensions in this order
    1. `.pyd` and `.dll` (Windows) or `.so` (Unix)
    2. `.py` for python source files
        - Look for a `__pycache__` direcotry. If it exists, look for `<tag>.pyc`
          where `<tag>` is a string specific to the current python version
    3. `.pyc` for bytecode-compiled python modules
    4. If module is `M` look in `M/__init__.py`
* python allows circular imports but you should avoid them
    - > If you keep a circular import, you must carefully manage the order in
      > which each module binds its own globals, imports other modules, and
      > accesses globals of other modules. You get greater control over the
      > sequence in which things happen by grouping your statements into
      > functions, and calling those functions in a controlled order, rather
      > than just relying on sequential execution of top-level statements in
      > module bodies.
* You can rebind `__import__` to create a custom importer but it's crude -
  import hooks are better
* Python offers import hooks to let you customise how importing works e.g. load
  code from different kinds of archives, from a DB, over the network etc. - see
  PEP 451 for gory details if you need them

- top level script is called "main program"
    - python will not cache the bytecode for it to disk
    - module name is `__main__`
        ```python
        sys.modules["__main__"]
        ```
    - common idiom to test if script is being invoked directly is to
        ```python
        if __name__ == '__main__':
            # do main prog stuff, maybe run tests
        ```

## Packages

- A package is a module containing other modules
- Package `foo` lives in a subdirectory `foo` off some directory on `sys.path`
  (usually the cwd of the main program)
- `foo/__init__.py` must exist except if the package is a "namespace package"
    - this file contains the module of the package and is often/usually empty
    - this file is loaded first when you load the package
    - other `.py` files in `foo/` are the modules of package `foo`
    - subdirectories of `foo/` which contain `__init__.py` files are subpackages
      of `foo`
        - nesting can go to any depth
- Packages can also liv ein zip files
- Package modules have additional dunder attrs
    - `__package__` name of package
    - `__path` path to the directory of the package

## Distribution

- many options
    - compressed archives of the usual types
    - self installing executuables
    - OS packages
    - wheels
- `pip` installed from PyPI repository of packages
- see [setup.md](./setup.md) for details

## Exceptions

```python
# similar to ruby, first except handler that matches is run, does not fall through to other except clauses
try:
    # risky code
except KeyError as ex:
    # handle ex
except (MyError, OtherError) as ex: # exceptions can be in a tuple
except Exception as ex:
    # handle ex
except:
    # DANGEROUS don't use, equivalent to `rescue Exception` in ruby - captures
    # exceptions you won't want to capture - use `Exception` instead (similar to
    # `StandardError` in Ruby)
else:
    # no exception raised
finally:
    # run in all cases
    # don't use following keywords in here because they stop exception
    # propagation and that is usually surprising in a finally clause:
    #   continue
    #   break
    #   return


# ...
except SomeEx as ex1:
    raise # re-raise ex1
    raise OtherException("msg") # raise a new exception
except SomeEx as ex2:
    # raise a new MyException with the __cause__ set to ex2
    raise MyException("my msg") from ex2
# ...
```

- Python seems to be surprisingly ok with exceptions as flow control e.g.
  iterators raise a `StopIteration` exception when they are done
- `sys.exc_info` or `sys.exception` (latter only 3.11+) are equivalient of `$!`
  in Ruby

### with and context managers

- statements within a `with` clause are run between a setup and teardown
  functions defined by the context object passed to with
- in Ruby this would be just passing a block to a method, python has dedicated
  syntax and a bit more ceremony
- intended for when you need to do setup and teardown around a chunk of code
    - or want to re-use that setup and teardown in many places
    - a more re-usable alternative to `try ...finally`
- context manager classes
    - defined `__enter__()` and `__exit__(type, value, traceback)`
    - often do not follow the initial cap naming convention for classes (hmmm)
- Python has a decorator in `@contextlib.contextmanager` to help you create a
  context manager class from a generator method (generators includes a `yield`
  statement)
- Python error output shows the nested exceptions which is nice

```python
# Since 3.10+ multiple context managers for a with statement can be enclosed in parentheses
with (expression [as varname], ...):
    statement(s)

# the above with is semantically the same as:

_normal_exit = True
_manager = expression
varname = _manager.__enter__()
try:
    statement(s)
except:
    _normal_exit = False
    if not _manager.__exit_(*sys.exc_info()):
        raise
    # note that exception does not propagate if __exit__ returns
    # a true value
finally:
    if _normal_exit:
        _manager.__exit__(None, None, None)
```

Exception heirarchy

```python
BaseException
  GeneratorExit
  KeyboardInterrupt
  SystemExit
  Exception
    AssertionError, AttributeError, BufferError, EOFError,
    MemoryError, ReferenceError, OsError, StopAsyncIteration,
    StopIteration, SystemError, TypeError
    ArithmeticError (abstract)
      OverflowError, ZeroDivisionError
    ImportError
      ModuleNotFoundError, ZipImportError
    LookupError (abstract)
      IndexError, KeyError
    NameError
      UnboundLocalError
    OSError
      ...
    RuntimeError
      RecursionError
      NotImplementedError
    SyntaxError
      IndentationError
        TabError
    ValueError
      UnsupportedOperation
      UnicodeError
        UnicodeDecodeError, UnicodeEncodeError,
        UnicodeTranslateError
    Warning
      ...
```

Exception attributes and methods

```python
ex.__cause__ # the nested exception

# list of notes added to exceptioin - caution: does not exist if no notes added
# (strange choice). The safe way to access this list is with getattr(exc, '__notes__', []).
ex.__notes__

ex.add_note('my note')

# returns a whole new exception with the given traceback
# handy for test frameworks etc. to remove themselves from a traceback
# TODO: what type is a traceback? list(str)?
ex.with_traceback(new_traceback)

# a reference to the exception that was active when ex was created
ex.__context__
```

You can create custom exceptions as in Ruby

```python
class MyException(Exception):
    """A doc explaining this exception"""
    # pass # you can use pass but it is better style to use a docstring

# you can use multiple inheritance
# they seem to recommend this over creating a heirarchy of exceptions - this feels wrong to me. Is it idiomatic?
class MyException(SomeException, AttributeError):
    """A doc explaining this exception"""
    # pass # you can use pass but it is better style to use a docstring
```

You can raise more than one exceptin at wonce using an `ExceptionGroup` (it's in
3.11+ only). `except*` will continue looking for matching exceptions unlike
`except` which only matches once

```python
class TooLongError(Exception):
    pass
class MisspelledError(Exception):
    pass
class GrammarError(Exception):
    pass

try:
    exceptions = []
    if (input.is_too_long()):
        exceptions.append(TooLongError())
    if (input.is_misspelled()):
        exceptions.append(MisspelledError())
    if (input.has_bad_grammar()):
        exceptions.append(GrammarError())

    if exceptions:
        raise ExceptionGroup("Found validation errors", exceptions)
except* GrammarError as ex:
    # notice that except* will rescue any of the exceptions in the group
except* TooLongError as ex:
    # notice that except* will rescue any of the exceptions in the group
```

Python is culturally into trying code in a `try` block and handling exceptions
rather than trying to avoid error states before you try the code

## Logging

Python `logging` module only supports the old-style `%s` formatting specifiers

## Assert

- when you run python with `-o` to optimize the assert statements are compiled
  away
- `assert condition` raises `AssertionError

```python
#  with`-o` assert statements compile away and `__debug__` is true. You can use
#  this to have some code run only in "debug mode"
if __debug__:
  # optimisations turned off
```

## Generators

- have `throw

```python
g = ... # a generator

# behaves as if an exception was raised at the `yield` statement in the generator
g.throw(exception_ob)

g.close() # equivalent to g.throw(GeneratorExit())
```

## Type annotations

- still an active work in progress since 3.5 and gets improved with each release
    - 3.12 looks to include a number of improvements here
- type annotations are **not enforced at runtime**
- python seems to have more tooling for types than ruby, a more robust embrace
  of types from the community
- types can be inline or in separate `.pyi` "interface" files
    - instagram has a tool to do type inference as your code runs, stores
      results in `.pyi` files
    - Google has tool to inline `.pyi` files into the python code
- Vscode seems to have it's own built-in type checker (pylance)
- Gotchas
    - Lambdas do not accept type annotations

```python
import typing # types come from here, you can use them as

# declare an int in the current module
count: int

# types use [] brackets
counts: list[int] = []
data: dict[str, tuple[int, int, str]]

# callable, takes a single str/bytes arg and returns a bool
my_func: typing.Callable[[str | bytes], bool] # can use | since 3.10
```

## General tips

- Use a comment instead of `pass` whenver you can - either will make python
  happy
- New versions of Python released every October, support lifetime seems similar
  to Ruby

## Progress

CHAP 1: done CHAP 2: done CHAP 3: up to tuples CHAP 4: TODO CHAP 5: TODO CHAP 6:
done CHAP 7: done

## Questions

```
Built-in types seem to have some methods hanging off them.
    When are things are method and when are they a separate function?
    Is there a guiding principle?
How promptly do people upgrade apps to new versions of python
```
