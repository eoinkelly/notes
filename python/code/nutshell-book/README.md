# Creating a new project

```bash
# check your python version
python --version

# check your poetry install is happy
poetry --version
poetry self update

# create new project
poetry new my-project

cd my-project

# install dev tools
poetry add --group dev black isort mypy flake8 safety

# run dev tools
poetry run black .
poetry run mypy .
poetry run flake8 .
poetry run isort .

poetry run safety generate policy_file # optional

# safety does not yet support poetry.lock out of the box
# https://github.com/pyupio/safety/issues/201
poetry export | poetry run safety check --stdin
```

## Useful VSCode extensions for Python

* https://marketplace.visualstudio.com/items?itemName=ms-python.python
* https://marketplace.visualstudio.com/items?itemName=ms-python.black-formatter
    * You need to configure it manually in your vscode settings
    * Bundles black so it might not be exactly the same version as you run on CLI
* https://marketplace.visualstudio.com/items?itemName=ms-python.mypy-type-checker
    * Bundles mypy so it might not be exactly the same version as you run on CLI