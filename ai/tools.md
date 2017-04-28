Good comparision of available libs: https://medium.com/@ricardo.guerrero/deep-learning-frameworks-a-review-before-finishing-2016-5b3ab4010b06#.hcp20j2se


Open source Libs

* Machine learning
    * Torch7
        * http://torch.ch/
        * a scientific computing framework
        * uses lua for scripting
        * can be used for machine learning, computer vision
          > Torch is a scientific computing framework with wide support for
          > machine learning algorithms that puts GPUs first. It is easy to use and
          > efficient, thanks to an easy and fast scripting language, LuaJIT, and an
          > underlying C/CUDA implementation
    * Deep learning
        * Tensor flow
            * https://www.tensorflow.org/
            * a lib for numerical computation using data flow graphs
            * is quite low level
        * Keras https://keras.io/
            * can use either keras or theano
            * higher level than either of them
        * theano
            > Theano is a Python library that allows you to define, optimize, and evaluate mathematical expressions involving multi-dimensional arrays efficiently.
    * Pytorch
        * a replacement for numpy that leverages GPUs well
        * python based
        * more granular than tensorflow
          > Usually one uses PyTorch either as:
          > 1. A replacement for numpy to use the power of GPUs.
          > 2. a deep learning research platform that provides maximum flexibility and speed

* Anaconda
    * aimed at data science teams
    * has its own package manager called 'conda'
    * bundles
      * python
      * the most popular python sci packages: scikit-learn, tensorflow, notebooks
      * R
      * storage: hadoop, json files, csv files, spark etc.
    > Anaconda is the leading open data science platform powered by Python. The open source version of Anaconda is a high performance distribution of Python and R and includes over 100 of the most popular Python, R and Scala packages for data science.
    >
    > Additionally, you'll have access to over 720 packages that can easily be installed with conda, our renowned package, dependency and environment manager, that is included in Anaconda.

# Running ML and AI workloads in the cloud

It seems both Google and AWS will offer instances aimed specifically at machine learning workloads

Environments for running machine learning

* Amazon has announced they will provide FPGA support
    * https://aws.amazon.com/marketplace/pp/B01M0AXXQB#product-description
* Google
    * will likely offer GPU support
    * has a REST api with some very high level APIs -  https://cloud.google.com/vision/
    * also has a ML cloud similar to AWS
        * https://cloud.google.com/ml/


# languages

* Python seems to be the dominant language for machine learning work
* R is the second most popular (anaconda bundles both)
* libs are often written in C++ but have interfaces for the higher level languages

## Python tooling

```
# the python tooling stack

| ----------------------------------------------- |
| scikit-learn (data mining & analysis tools)     |
| ----------------------------------------------- |
| numpy (fast N-dimentional arrays) | scipy (algorithms) | mathplotlib (2D plotting lib) |
| ----------------------------------------------- |
```

* scikit-learn
    * builds on top of numpy, scipy (the lib) and matplotlib
* scipy
    * a collection of algorithms
    * is both a library and the umbrella project for other python scientific stuff
* matplotlib
    * 2D plotting library
* NumPy
    * is the fundamental package for scientific computing with Python. It
      contains among other things
    * a powerful N-dimensional array object
    * sophisticated (broadcasting) functions
    * tools for integrating C/C++ and Fortran code
    * useful linear algebra, Fourier transform, and random number capabilities
