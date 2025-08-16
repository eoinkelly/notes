# type coercion

- C _arithmetic operators_ cannot work on a mix of int and float so will convert
  int to float
    - consequences
        - integer division truncates

```
int operator int // no conversion, operator operates directly on integers
float operator float // no conversion, operator operates directly on floats
int operator float // int silently converted to float before operation, operator operates directly on floats
```
