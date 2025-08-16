# Python packages for data science

- [Python packages for data science](#python-packages-for-data-science)
    - [pytorch vs tensorflow vs keras](#pytorch-vs-tensorflow-vs-keras)
        - [Pros and Cons of PyTorch vs. TensorFlow](#pros-and-cons-of-pytorch-vs-tensorflow)
    - [pytorch](#pytorch)
    - [tensorflow](#tensorflow)
    - [keras](#keras)
    - [ipykernel](#ipykernel)
    - [pandas](#pandas)
    - [scikit-learn](#scikit-learn)
    - [scipy](#scipy)
    - [matplotlib](#matplotlib)
    - [numpy](#numpy)
    - [plotly](#plotly)
    - [theano](#theano)
    - [Cython](#cython)
    - [Numba](#numba)

?? should I just use conda instead of python+pip for data science stuff? it
seems very popular

## pytorch vs tensorflow vs keras

Tensorflow is probably the best for building an AI product vs doing research.

- there are two main contenders for ML framework now: keras+tensorflow vs
  pytorch
- tensorflow has better visualisation and ability to run models in production
  (tensorflow serving framework)
- tensorflow is probably still the standard but pytorch is gaining popularity
- deploying to production
    - tensorflow has "serving framework" which provides a Rest client API
    - pytorch doesn't have any framework so you have to use django or flask
      (this might not be a bad thing)
- pytorch is faster and more debuggable than tensorflow
- pytorch is faster and more debuggable than keras
- keras may be more "plug and play" for developers vs data scientists
-

### Pros and Cons of PyTorch vs. TensorFlow

From https://builtin.com/data-science/pytorch-vs-tensorflow

> PyTorch optimizes performance by taking advantage of native support for
> asynchronous execution from Python. In TensorFlow, you’ll have to manually
> code and fine tune every operation to be run on a specific device to allow
> distributed training. However, you can replicate everything in TensorFlow from
> PyTorch but you need to put in more effort.
>
> When it comes to visualization of the training process, TensorFlow takes the
> lead. Visualization helps the developer track the training process and debug
> in a more convenient way. TenforFlow’s visualization library is called
> TensorBoard. PyTorch developers use Visdom, however, the features provided by
> Visdom are very minimalistic and limited, so TensorBoard scores a point in
> visualizing the training process.

- PYTORCH PROS:
    - Python-like coding.
    - Dynamic graph.
    - Easy and quick editing.
    - Good documentation and community support.
    - Open source.
    - Plenty of projects out there using PyTorch.
- PYTORCH CONS:
    - Third-party needed for visualization.
    - API server needed for production.
- TENSORFLOW PROS:
    - Simple built-in high-level API.
    - Visualizing training with Tensorboard.
    - Production-ready thanks to TensorFlow serving.
    - Easy mobile support.
    - Open source.
    - Good documentation and community support.
- TENSORFLOW CONS:
    - Static graph.
    - Debugging method.
    - Hard to make quick changes.

## pytorch

- developed by Facebook
- a replacement for numpy that can run workloads on GPUs

## tensorflow

- originally from Google

## keras

- https://keras.io/about/
- Uses tensorflow as a backend
- designed to allow quick experimentation
- recently merged into tensorflow - Keras is the high-level API of the
  tensorflow stack
- Keras comes packaged with TensorFlow 2 as tensorflow.keras. To start using
  Keras, simply install TensorFlow 2.

## ipykernel

- required to be a kernel for Jupyter notebooks

## pandas

- https://pandas.pydata.org/
- provides two main data structures;
    1. Series
    2. DataFrame

## scikit-learn

- builds on top of numpy, scipy (the lib) and matplotlib as a stack

```
|                         scikit-learn (data mining & analysis tools)                    |
| --------------------------------- | ------------------ | ----------------------------- |
| numpy (fast N-dimentional arrays) | scipy (algorithms) | mathplotlib (2D plotting lib) |
```

- scikit-learn
- matplotlib
- NumPy

## scipy

- a collection of algorithms
- is both a library and the umbrella project for other python scientific stuff

## matplotlib

- 2D plotting library

## numpy

- is the fundamental package for scientific computing with Python. It contains
  among other things
- a powerful N-dimensional array object
- sophisticated (broadcasting) functions
- tools for integrating C/C++ and Fortran code
- useful linear algebra, Fourier transform, and random number capabilities

## plotly

- https://plotly.com/python/
- a graphing library, competes with R-shiny

> Plotly's Python graphing library makes interactive, publication-quality
> graphs. Examples of how to make line plots, scatter plots, area charts, bar
> charts, error bars, box plots, histograms, heatmaps, subplots, multiple-axes,
> polar charts, and bubble charts.

## theano

> Theano is a Python library that allows you to define, optimize, and evaluate
> mathematical expressions involving multi-dimensional arrays efficiently.

## Cython

- https://cython.readthedocs.io/en/latest/src/quickstart/overview.html
- A superset of python which compiles down to C/C++ code that can be loaded as
  python extension modules

> Cython is a programming language that makes writing C extensions for the
> Python language as easy as Python itself. It aims to become a superset of the
> Python language which gives it high-level, object-oriented, functional, and
> dynamic programming. Its main feature on top of these is support for optional
> static type declarations as part of the language. The source code gets
> translated into optimized C/C++ code and compiled as Python extension modules.
> This allows for both very fast program execution and tight integration with
> external C libraries, while keeping up the high programmer productivity for
> which the Python language is well known.

## Numba

- http://numba.pydata.org/
- an open source JIT compiler that translates a subset of Python and NumPy code
  into fast machine code using LLVM
- you use it by applying one of the numba decorators to your code e.g. `@njit`
