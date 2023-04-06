# tensorflow on GPU on macOS M1

https://developer.apple.com/metal/tensorflow-plugin/ uses conda to install the tensorflow-metal plugin.

```bash
$ cd ./conda-home
$ pyenv local conda
$ conda install -c apple tensorflow-deps
$ pip install tensorflow-macos
$ pip install tensorflow-metal
$ python tensorflow_gpu_test.py
```

This didn't work for me.

```
RuntimeError: module compiled against API version 0x10 but this version of numpy is 0xf
RuntimeError: module compiled against API version 0x10 but this version of numpy is 0xf
ImportError: numpy.core._multiarray_umath failed to import
ImportError: numpy.core.umath failed to import
RuntimeError: module compiled against API version 0x10 but this version of numpy is 0xf
ImportError: numpy.core._multiarray_umath failed to import
ImportError: numpy.core.umath failed to import
RuntimeError: module compiled against API version 0x10 but this version of numpy is 0xf
ImportError: numpy.core._multiarray_umath failed to import
ImportError: numpy.core.umath failed to import
Traceback (most recent call last):
  File "/Users/eoinkelly/Dropbox/Eoin/Notes/on-github/python/conda-home/tensorflow_gpu_test.py", line 1, in <module>
    import tensorflow as tf
  File "/Users/eoinkelly/.pyenv/versions/anaconda3-2023.03/lib/python3.10/site-packages/tensorflow/__init__.py", line 37, in <module>
    from tensorflow.python.tools import module_util as _module_util
  File "/Users/eoinkelly/.pyenv/versions/anaconda3-2023.03/lib/python3.10/site-packages/tensorflow/python/__init__.py", line 42, in <module>
    from tensorflow.python import data
  File "/Users/eoinkelly/.pyenv/versions/anaconda3-2023.03/lib/python3.10/site-packages/tensorflow/python/data/__init__.py", line 21, in <module>
    from tensorflow.python.data import experimental
  File "/Users/eoinkelly/.pyenv/versions/anaconda3-2023.03/lib/python3.10/site-packages/tensorflow/python/data/experimental/__init__.py", line 97, in <module>
    from tensorflow.python.data.experimental import service
  File "/Users/eoinkelly/.pyenv/versions/anaconda3-2023.03/lib/python3.10/site-packages/tensorflow/python/data/experimental/service/__init__.py", line 419, in <module>
    from tensorflow.python.data.experimental.ops.data_service_ops import distribute
  File "/Users/eoinkelly/.pyenv/versions/anaconda3-2023.03/lib/python3.10/site-packages/tensorflow/python/data/experimental/ops/data_service_ops.py", line 22, in <module>
    from tensorflow.python.data.experimental.ops import compression_ops
  File "/Users/eoinkelly/.pyenv/versions/anaconda3-2023.03/lib/python3.10/site-packages/tensorflow/python/data/experimental/ops/compression_ops.py", line 16, in <module>
    from tensorflow.python.data.util import structure
  File "/Users/eoinkelly/.pyenv/versions/anaconda3-2023.03/lib/python3.10/site-packages/tensorflow/python/data/util/structure.py", line 22, in <module>
    from tensorflow.python.data.util import nest
  File "/Users/eoinkelly/.pyenv/versions/anaconda3-2023.03/lib/python3.10/site-packages/tensorflow/python/data/util/nest.py", line 34, in <module>
    from tensorflow.python.framework import sparse_tensor as _sparse_tensor
  File "/Users/eoinkelly/.pyenv/versions/anaconda3-2023.03/lib/python3.10/site-packages/tensorflow/python/framework/sparse_tensor.py", line 25, in <module>
    from tensorflow.python.framework import constant_op
  File "/Users/eoinkelly/.pyenv/versions/anaconda3-2023.03/lib/python3.10/site-packages/tensorflow/python/framework/constant_op.py", line 25, in <module>
    from tensorflow.python.eager import execute
  File "/Users/eoinkelly/.pyenv/versions/anaconda3-2023.03/lib/python3.10/site-packages/tensorflow/python/eager/execute.py", line 21, in <module>
    from tensorflow.python.framework import dtypes
  File "/Users/eoinkelly/.pyenv/versions/anaconda3-2023.03/lib/python3.10/site-packages/tensorflow/python/framework/dtypes.py", line 37, in <module>
    _np_bfloat16 = _pywrap_bfloat16.TF_bfloat16_type()
TypeError: Unable to convert function return value to a Python type! The signature was
        () -> handle
```