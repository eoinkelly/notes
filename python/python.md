Package management

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
