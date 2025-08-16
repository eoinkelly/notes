# Tensor flow

- https://www.tensorflow.org/get_started/get_started
- provides multiple apis
    1. Tensorflow Core
        - recommended for ML researchers
        - gives fine grain control
    2. Higher level APIs
        - ++ easier to learn
        - high level APIs with `contrib` in name are still in development
            > A high-level API like tf.contrib.learn helps you manage data sets,
            > estimators, training and inference
    3. Keras

```py
# Aside: append `.` to a number to make it a float in python

3. # rank: 0, shape []
[1., 3., 5.] # rank: 1, shape [3]
[[1., 3., 5.], [2., 4., 6.]] # rank: 2, shape [2, 3]

[[[1., 2., 3.]], [[7., 8., 9.]]] # rank: 3, shape: [2, 1, 3]
# or
a = [1., 2., 3.]
b = [7., 8., 9.]
c = [a]
d = [b]
[c, d]
```

- Tensorflow programs contain 2 specializations
    1. build a computational graph of tensors
    2. run that graph

* Each node takes 0 or more tensors as inputs and products 1 tensor as output
* the graph has "operations" which are also nodes (i.e. they take in tensors and
  product one tensor)

> TensorFlow provides optimizers that slowly change each variable in order to
> minimize the loss function. The simplest optimizer is gradient descent.

## MNIST

- https://www.tensorflow.org/get_started/mnist/beginners
- MNIST is the "hello world" of ML
- http://yann.lecun.com/exdb/mnist/
