# Statistics

### Average

> In statistics, the term average refers to any of the measures of central
> tendency

_Average_ is an imprecise term and should be avoided in contexts where you need
precision.

### Arithmetic mean (also known as 'mean')

In the interest of being precise, we try to always remember whether our data-set
is a full population or a statistical sample (a subset of the population).

- If the dataset is a population, then we calculate the _population mean_.
- If the dataset is a sample then we calculate the _sample mean_.

General

- Symbol is $\overline{x}$
- Can be used to report _central tendencies_ but is very influenced by
  outliers - if the distrubtion is skewed it may be better to use median

For a collection of $n$ values:

$$\overline{x} = \frac{1}{n} \sum_{i=1}^{n} x_i $$

| Math concept     | Spreadsheet equation name |
| ---------------- | ------------------------- |
| Arithemetic mean | AVERAGE                   |

### Geometic mean

| Math concept   | Spreadsheet equation name |
| -------------- | ------------------------- |
| Geometric mean | GEOMEAN                   |

### Harmonic mean

| Math concept  | Spreadsheet equation name |
| ------------- | ------------------------- |
| Harmonic mean | HARMEAN                   |

### median

more "robust" than the arithmetic mean

### standard deviation

1. population stddev $\sigma$
2. sample stddev $S$

### r value

- a measure of how well a given data-set corresponds to a _linear_ model.
- is based on stddev so is it senstive to outliers?

Q: what to do about outliers before you run things like std-dev?

## Visualing data

- Scatter plots
    - by convention we put the independent variable on the X axis
        - => the choice of variable for the X and Y axis tell you something
          about what the creator of the graph thought was the dependent variable
          (in cases where it's not obvious)

- Histograms
