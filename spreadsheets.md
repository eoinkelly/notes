
Ranges

There are ? types

1. Adjacent/Symmetrical
1. Non-adjacent/Irregular




Adjacent/Symmetrical range

    {top-left-cell}:{bottom-right-cell}
    A1:C12
    A:A # sum the whole A column

* omit the row number from both parts and Excel will consider it the entire column

* is "rectangular"

> n Excel, a range is defined by the reference of the upper left cell (minimum value) of the range and the reference of the lower right cell (maximum value) of the range


Eventually separate cells can be added to this selection, then the range is called an irregular cell range.

In Excel, the minimum and maximum value are included.

```


A1:A10 # Includes A1 -> A10
```

Non-adjacent range

Seems to either use + or ; as separator
```

A1+A3+B5 # includes the cells A1, A3, B5 only
A1;A3;B5 # includes the cells A1, A3, B5 only
```
