# Pry


* in pry as in ruby you are always in a scope
    * class, module, ??? are the only keywords which change scope in ruby

* ruby has the notion of
    * current object
        * method calls without explicit receivers start in this object
        * self points at it
    * current class
        * constant lookups start here
        * there is no equivalent of the `self` keyword
        * when you define a method it gets added to the _current class_

When you start pry (or IRB) these are

* current object = main
* current class = Object

```ruby
pry> self
main
pry> self.class
Object < BasicObject
```

implicaitons
* when you create methods outside a class in ruby they are added to Object

Start

## inspecting state

pry(main)> ls
Rails::ConsoleMethods#methods: app  controller  helper  new_session  reload!
self.methods: inspect  to_s
locals: _  __  _dir_  _ex_  _file_  _in_  _out_  _pry_


rails-pry(main)> ls
self.methods: inspect  to_s
locals: _  __  _dir_  _ex_  _file_  _in_  _out_  _pry_  err



# showing constants

    ls -c

* shows constants in the current scope
    * blue = instance of Class
    * purple = instance of Exception
    * yellow = a constant pending autoload i.e. `autoload?(:ConstName)` is truthy


## Aside: ruby autoload

* `autoload` is like a delayed `require`

```ruby
# autoload(:Symbol_of_constant_name, "arg for require")
autoload :Foo, "things/foo"

puts "foo is awaiting autoloading" if autoload?(:Foo)

# autoload?(:Constant_as_symbol)
# returns string of the constant if it is is loaded, nil if it is not loaded

# this will trigger the require
Foo
```

TODO
http://www.rubyinside.com/ruby-techniques-revealed-autoload-1652.html
indicates that there might be problems with autoload and `require` and threads at high load
 autoload works in a similar way to require, but it only loads the file specified when a constant of your choosing is accessed/used for the first time.
