# A good setup for python in 2023+

Recommendations for setting up a new local Python development env in 2023:

-   Use `pyenv` to manage multiple Python versions
-   Use latest stable Python
-   Use [poetry](https://python-poetry.org/) for dependency management **and** packaging
-   Use [black](https://pypi.org/project/black/) accepting all defaults for formatting
-   Use [isort](https://pycqa.github.io/isort/) to automatically sort imports
-   In your code always:
    -   Use type hinting where possible
    -   Use data classes where sensible
-   In your code maybe:
    -   use mypy https://mypy-lang.org/ (Typescript:Javascript as MyPy:Python)
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

## setuptools

-   Installed by default with Python
-   Summary: _Easily download, build, install, upgrade, and uninstall Python packages_
-   https://discuss.python.org/t/pip-without-setuptools-could-the-experience-be-improved/11810/2
    -   it will help pip install legacy packages

TODO: not sure what it does?

## virtualenv

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