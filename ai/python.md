
* `(a, b, c)` creates a tuple in python (tuples are immutable, value objects, efficent because they aren't copied around)
* `[a, b, c]` creates a list in python (lists are mutable)


```python
import numpy as np

# it seems tuples and lists can be used interchangeably to pass args to numpy functions
np.matrix( ((1,2), (3,4)) ) # notice this is a tuple of tuples (np.array expects one arg)
np.array([[100,175,210],[90,160,150],[200,50,100],[120,0,310]]) # notice this is a list of lists (np.array expects one arg)

```

```python
>>> import numpy as np
>>> x = np.array([1,2])
>>> y = np.array([2,4])
>>> x + y
array([3, 6])

>>> 2 * x
array([2, 4])

>>> np.dot(x,y)
10

>>> x * y
array([2, 8])

>>> type(x)
<class 'numpy.ndarray'>

>>> type(x * y)
<class 'numpy.ndarray'>

>>> a = np.array(2,3) # elemetns must be grouped in arg
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: Cannot interpret '3' as a data type
>>> a = np.array((2,3))
>>> a
array([2, 3])
>>> a2 = np.array([2,3])
>>> a2
array([2, 3])
>>> a == a2
array([ True,  True])
>>> b = np.array((1,2), (3,4)) # failed because we gave two separate tuples
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: Cannot interpret '3' as a data type
>>> b = np.array( ((1,2), (3,4)) ) # works because we passed a tuple containing two tuples
>>> b
array([[1, 2],
       [3, 4]])
>>> b_matrix = np.matrix( ((1,2), (3,4)) )
>>> b_matrix
matrix([[1, 2],
        [3, 4]])
>>> type(b_matrix)
<class 'numpy.matrix'>
>>> type(b)
<class 'numpy.ndarray'>
>>> np.dot(b, b_matrix)
matrix([[ 7, 10],
        [15, 22]])
>>> np.mat(x)
matrix([[1, 2]])
>>> np.mat(x) # cast ndarray to matrix
matrix([[1, 2]])
>>> u = (1,2)
>>> type(u)
<class 'tuple'>
>>> v = [1,2]
>>> type(v)
<class 'list'>
>>> u[0]
1
>>> v[1]
2
>>> v[1] = 12
>>> v
[1, 12]
>>> u[1]
2
>>> u[1] = 12 # tuples are immutable
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: 'tuple' object does not support item assignment
>>>
```
