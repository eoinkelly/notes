
## Arrays

* name of array is a pointer to its first element
    * implications
        * when array is passed to function the pointer is copied but not the array contents
* array declaration includes 1)element type and 2)no. of elements to allow the compiler to reserve storage

## Arrays

* are homogenous (all items must be same type)
* 0 based index
* must be sized when created
* defined by giving
    1. type
    2. name
    3. number of elements
* The name of the array is a pointer to its first element.
    * `=>` arrays are always passed to functions by reference
* Used to implement strings in C (string is an array of characters terminated by `\0` (null byte))

TODO: play with passing an array by value

```c
// type name[num-elements]
int ages[11];
```
