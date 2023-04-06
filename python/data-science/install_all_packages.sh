#!/bin/bash

set -x
set -e

# upgrade pip (is often required when you first install a new python version)
pip install --upgrade pip

# required to run jupyter notebooks in VSCode
pip install --upgrade ipykernel

# install code formatting tools
pip install --upgrade black pylint

# as of 2023-04-07 there isn't an easy way to install tensorflow on M1 macOS with pip

# Install common packages for data science in Python
pip install numpy  --upgrade
pip install pandas  --upgrade
pip install matplotlib  --upgrade
pip install scikit-learn  --upgrade
pip install scipy  --upgrade
pip install plotly  --upgrade

pip install torch torchvision --upgrade