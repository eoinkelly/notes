# A good setup for python in 2023+

- [A good setup for python in 2023+](#a-good-setup-for-python-in-2023)
  - [Recommendations for setting up a new local Python development env in 2023:](#recommendations-for-setting-up-a-new-local-python-development-env-in-2023)
  - [My local python setup](#my-local-python-setup)
  - [pip](#pip)
  - [poetry](#poetry)
  - [setuptools package](#setuptools-package)
  - [wheel package](#wheel-package)
  - [Virtual environments](#virtual-environments)
    - [venv](#venv)
    - [virtualenv (old, deprecated)](#virtualenv-old-deprecated)
  - [pylint](#pylint)
  - [Anaconda](#anaconda)
  - [Pypy](#pypy)
  - [PyPI (Python Package Index)](#pypi-python-package-index)
  - [Stackless](#stackless)

## Recommendations for setting up a new local Python development env in 2023:

-   Use `pyenv` to manage multiple Python versions
-   Use latest stable CPython
-   Use [poetry](https://python-poetry.org/) for dependency management **and** packaging
-   Use [black](https://pypi.org/project/black/) accepting all defaults for formatting
-   Use [isort](https://pycqa.github.io/isort/) to automatically sort imports
-   In your code always:
    -   Use type hinting where possible
    -   Use data classes where sensible
-   In your code maybe:
    -   use mypy https://mypy-lang.org/ (mypy will statically check the type hints you add to your code)
-   Maybe use pylint but we don't have a standard setup for it

## My local python setup

-   pyenv
    -   installed via homebrew
    -   installing to `~/.pyenv`
    -   Global version set in file `.pyenv/version` via command `pyenv global 3.whatever`
    -   macOS system python installed but best ignored
    -   I install the following packages after downloading new python:
        ```
        pip install neovim
        ```
    -   I set `export PYTHONDONTWRITEBYTECODE=1` in `.zshenv` to stop Python writing `__pycache__` dirs everywhere
-   pip
    -   installed with the python version
    -   I upgraded it when it tells me to
    -   I have an env var set to point it at `~/.pip.conf` for config (currently nothing in there)
-   poetry

    -   installed via their recommended installer
    -   post install stuff I do

        ```bash
        poetry add black
        poetry run black .

        poetry add isort
        poetry run isort .
        ```

## pip

-   Installed by default with Python
-   Summary _The PyPA recommended tool for installing Python packages_
-   Packages installed with pip are global to all apps using that version of Python

```sh
pip list # show packages
pip inspect # show details of current python environment
pip install --upgrade mypkg # upgrade an installed package
pip show mypkg # show package details
```

## poetry

-   dependency management and packaging (think gem + bundler)
-   has a custom installer script
-   like npm has dependencies and dev-dependencies
-   uses virtualenvs under the hood
-   uses a TOML config file
-   creates a TOML lockfile when you `poetry install`
-   has dependency groups like bundler
-   you have to prefix your commands with `poetry run` e.g.

    ```bash
    poetry run python myscript.py
    poetry run black
    poetry run pytest

    # OR you can use a nested shell
    poetry shell
    poetryshell$

    poetry install # install deps
    ```

## setuptools package

-   Installed by default with Python
-   Summary: _Easily download, build, install, upgrade, and uninstall Python packages_
-   https://discuss.python.org/t/pip-without-setuptools-could-the-experience-be-improved/11810/2
    -   it will help pip install legacy packages

> While pip alone is sufficient to install from pre-built binary archives, up to
> date copies of the setuptools and wheel projects are useful to ensure you can
> also install from source archives

## wheel package

-   Installed by default with Python

> While pip alone is sufficient to install from pre-built binary archives, up to
> date copies of the setuptools and wheel projects are useful to ensure you can
> also install from source archives

## Virtual environments

https://packaging.python.org/en/latest/tutorials/installing-packages/#id16

-   venv is available by default in Python 3.3 and later, and installs pip and setuptools into created virtual environments in Python 3.4 and later.
-   virtualenv needs to be installed separately, but supports Python 2.7+ and Python 3.3+, and pip,
-   setuptools and wheel are always installed into created virtual environments by default (regardless of Python version).

### venv

-   replacement for `virtualenv`
-   stubs call your installed version of python binary
-   gives you a localised python packages install

Q: where do people put their venv directories? within the project or in some separate heirarchy

> Many people keep virtual environment in directory named env, .venv, or venv
> directory within the project, but its recommended to keep virtual environments
> separate from the project code.
> https://medium.com/@sukul.teradata/understanding-python-virtual-environments-using-venv-and-virtualenv-283f37d24b13

```bash
# setup the venv
mkdir venv_example
# there is no cli stub for venv - you are supposed to call it like this
python -m venv ./venv_example

# source it (could be a diff terminal in a totally diff directory
source venv_example/bin/activate
```

Changes venv made to my shell environment when I activated it:

1. Prepended `venv_example/bin` to my path
    - this folder contains stubs for `python` and `pip`
2. Set new env var `VIRTUAL_ENV=/full/path/to/venv_example`

### virtualenv (old, deprecated)

-   only use if your python is older than 3.3 (where `venv` was introduced)
-   https://virtualenv.pypa.io/en/latest/
-   creates isolated python environments
-   a subset of it is packaged with python in `venv` module but seems less flexible

```bash
$ pip install virtualenv
$ virtualenv path/to/some/dir
$ path/to/some/dir/bin/activate

# do somework

$ deactivate # put in shell by virtualenv
```

## pylint

-   think "rubocop" if coming from Ruby world
-   you need a `__init__.py` in whatever directory you point pylint at or it explodes
-   supports optional plugins
-   Some possible configs
    -   https://github.com/adafruit/Adafruit_Learning_System_Guides/blob/main/.pylintrc
    -   https://github.com/pylint-dev/pylint/blob/main/pylintrc (what they pylint project uses on their own code)

```
poetry run pylint .
```

## Anaconda

-   A complete distribution of Python + packages to do data science.
-   Has a more minimal version called `miniconda`

> Conda is an open-source package and environment management system that runs on
> Windows, macOS, and Linux. Conda quickly installs, runs, and updates packages
> and their dependencies. It also easily creates, saves, loads, and switches
> between environments on your local computer. It was created for Python programs,
> but it can package and distribute software for any language.

## Pypy

-   https://www.pypy.org/
-   Is faster than CPython in many ways

## PyPI (Python Package Index)

https://pypi.org/

-   There seem to be many ways a python can be packages
    -   Sometimes use `pyproject.toml`
    -   Sometimes uses a `setup.py` with a `./<pkg-name>.egg-info/` directory
-   Python packages can have a build step if they require compiled dependencies
-   The "src" package is basicallly what is in the git repo
-   Built packages are `.whl` wheel files and target a particular OS and architecture

## Stackless

https://en.wikipedia.org/wiki/Stackless_Python

> Although the whole Stackless is a separate distribution, its switching
> functionality has been successfully packaged as a CPython extension called
> greenlet.[5] It is used by a number of libraries (e.g. gevent[6]) to provide a
> green threading solution for CPython. Python since has received a native
> solution for green threads: await/async.
