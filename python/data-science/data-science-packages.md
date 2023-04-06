# Python packages for data science

- [Python packages for data science](#python-packages-for-data-science)
  - [pytorch vs tensorflow vs keras](#pytorch-vs-tensorflow-vs-keras)
    - [Pros and Cons of PyTorch vs. TensorFlow](#pros-and-cons-of-pytorch-vs-tensorflow)
  - [pytorch](#pytorch)
  - [tensorflow](#tensorflow)
  - [keras](#keras)
  - [ipykernel](#ipykernel)
  - [numpy](#numpy)
  - [pandas](#pandas)
  - [matplotlib](#matplotlib)
  - [scikit-learn](#scikit-learn)
  - [scipy](#scipy)
  - [plotly](#plotly)

?? should I just use conda instead of python+pip for data science stuff? it seems very popular

## pytorch vs tensorflow vs keras

Tensorflow is probably the best for building an AI product vs doing research.

-   there are two main contenders for ML framework now: keras+tensorflow vs pytorch
-   tensorflow has better visualisation and ability to run models in production (tensorflow serving framework)
-   tensorflow is probably still the standard but pytorch is gaining popularity
-   deploying to production
    -   tensorflow has "serving framework" which provides a Rest client API
    -   pytorch doesn't have any framework so you have to use django or flask (this might not be a bad thing)
-   pytorch is faster and more debuggable than tensorflow
-   pytorch is faster and more debuggable than keras
-   keras may be more "plug and play" for developers vs data scientists
-

### Pros and Cons of PyTorch vs. TensorFlow

From https://builtin.com/data-science/pytorch-vs-tensorflow

> PyTorch optimizes performance by taking advantage of native support for
> asynchronous execution from Python. In TensorFlow, you’ll have to manually code
> and fine tune every operation to be run on a specific device to allow
> distributed training. However, you can replicate everything in TensorFlow from
> PyTorch but you need to put in more effort.
>
> When it comes to visualization of the training process, TensorFlow takes the
> lead. Visualization helps the developer track the training process and debug in
> a more convenient way. TenforFlow’s visualization library is called TensorBoard.
> PyTorch developers use Visdom, however, the features provided by Visdom are very
> minimalistic and limited, so TensorBoard scores a point in visualizing the
> training process.

-   PYTORCH PROS:
    -   Python-like coding.
    -   Dynamic graph.
    -   Easy and quick editing.
    -   Good documentation and community support.
    -   Open source.
    -   Plenty of projects out there using PyTorch.
-   PYTORCH CONS:
    -   Third-party needed for visualization.
    -   API server needed for production.
-   TENSORFLOW PROS:
    -   Simple built-in high-level API.
    -   Visualizing training with Tensorboard.
    -   Production-ready thanks to TensorFlow serving.
    -   Easy mobile support.
    -   Open source.
    -   Good documentation and community support.
-   TENSORFLOW CONS:
    -   Static graph.
    -   Debugging method.
    -   Hard to make quick changes.

## pytorch

-   developed by Facebook

## tensorflow

- originally from Google

## keras

- https://keras.io/about/
- Uses tensorflow as a backend
- designed to allow quick experimentation
- recently merged into tensorflow - Keras is the high-level API of the tensorflow stack
- Keras comes packaged with TensorFlow 2 as tensorflow.keras. To start using Keras, simply install TensorFlow 2.

## ipykernel

-   required to be a kernel for Jupyter notebooks

## numpy

## pandas

## matplotlib

## scikit-learn

## scipy

## plotly
