# Applying the ABC metric to C, C++ and Java

<http://www.softwarerenovation.com/ABCMetric.pdf>

- original author proposed it as a way of measuring code _size_ that was better
  than SLOC
    - he did NOT propose it as a measure of software quality or complexity
- the paper has a good overview of the history of software quality metrics

Assumptions:

- this assumes that the scales of A, B, C should all be the same!
    - QUESTION: has there been any work done to give weightings to these?
- the counting has to be adjusted for the syntax of each programming language

Method

- A = assignment (or storage)
- B = branching (function calls, not logical branches)
- C = conditionals (if, else, unless etc.)

The metric is expressed as a tuple for a function/method `<A,B,C>` where

A = count of all assignments B = count of all branches C = count of all
conditionals

It is boiled down to a single no. by considering it to be a point in 3D space
and calculating the magnitude of its vector:

```
result = sqrt(A^2 + B^2 + C^2)
```
