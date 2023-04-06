# Jupyter

-   Jupyter is a web app which servers an UI allowing users to create notebooks and run them using a "kernel" on the server.
-   VSCode supports notebooks really well so we don't really need it for local development
-   It would be useful running as the UI to a server with enough grunt to do the training etc. you want to do - it's a much more human friendly alternative to editing python files over ssh.

## Notebooks in VSCode

-   You still need a Python environment with all the required packages installed.
-   I'm just installing them in my global pyenv set of packages. I presume using a virtualenv is possible but it's enough extra faff that I'll ignore it while I can

```bash
pip install --upgrade pip

# Install common packages for data science in Python
pip install numpy  --upgrade
pip install pandas  --upgrade
pip install matplotlib  --upgrade
pip install scikit-learn  --upgrade
pip install scipy  --upgrade
pip install plotly  --upgrade
```

## No tensorflow out of the box on macOS M1 as of 2023-04-02

```bash
pip install tensorflow
ERROR: Could not find a version that satisfies the requirement tensorflow (from versions: none)
ERROR: No matching distribution found for tensorflow
```

It seems you can make it work but you have to use miniconda (the mini version of Anaconda) https://caffeinedev.medium.com/how-to-install-tensorflow-on-m1-mac-8e9b91d93706

https://github.com/deganza/Install-TensorFlow-on-Mac-M1-GPU/blob/main/Install-TensorFlow-on-Mac-M1-GPU.ipynb
