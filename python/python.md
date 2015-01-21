# Source

* http://learnxinyminutes.com/docs/python/

# Package management

* setuptools
* pip
    * ships in 3.4 (not in anything previous to that!)
* easy_install
    * before `pip` this was the de-facto way to install packages
* ez_setup.py
    * a script that bootstraps `setuptools`
* PyPI
    * the python package index (their version of rubygems I think)

old school way

1. download a .tar.gz
1. run the `python ./package/setup.py install` to install it

Conclusion:

It's a hot mess but pip is all I need to care about I think


# General

Python objects are open like OpenStruct in ruby

Python lets you grab "method objects" from classes and use them anywhere python expects a "callable"

dir() shows you all the "attributes" in an object

You can make lambdas with `lambda:`

python can implement ruby' `method_missing` by overriding both of `__getattr__` and `__setattr__`
`__getattr__` is what python calls on an object to get its attribute
