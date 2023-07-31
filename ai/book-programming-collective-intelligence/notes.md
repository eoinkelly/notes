# Programming collective intelligence

## Preface

http://www.programmableweb.com/

## Chapter 1

* Machine learning is a sub section of AI concerned with algorithms which allow computers to learn
* All non-random data contains patterns
* If the machine can recognise the patterns it can _generalise_ about the data
* In order to generalise it trains a model with what it determines are the most important aspects of the data

An example of how humans generalise:

1. You get lots of emails
1. You recognise that emails which contain "online pharmacy" are usually spam
1. You generalise to say that all emails which contain "online pharmacy" are spam

Transparent vs Opaque ML algorithms

* Transparent
    * Some ML algorithms are **transparent** i.e an observer can totally understand how the computer came to its conclusion e.g. _decision tree_ algorithms.
* Opaque/black box
    * Others are **black box** i.e. it is very difficult/impossible to reproduce the reasoning behind the answer
      * Should I think of this kind of learning a bit like I think about "gut instinct" in humans - with a lot of experience it _can_ be good
      * Are opaque ML techniques to be trusted less?

Simple correlation analysis and regression are both basic forms of ML.

Algorithms have weaknesses

* they vary in their ability to  generalise over very large sets of patterns
* a pattern which is unlike any the algorithm has seen before is very likely to be misinterpreted
* machines can only generalise on data they have seen and even then in a very limited manner
* all machine learning methods suffer from a possibility of over generalising - as with most things in life, strong generalisations based on a few examples are rarely entirely accurate.

END CHAPTER 1

## Chapter 2

Collaborative filtering

1. Search a large group of people and find a smaller set with tastes similar to yourself
    * Problem #1: Find similar people
1. Look at the things those people like and present them as a ranked list of suggestions
    * Problem #2: Combine the things they like to create a ranked list of suggestions


_No matter how preferences are stored we need a way to map them onto numerical values_

* Do we? Why numerical?
  * ++ processing performance
      * ++ if you are going to use some values to represent real world things in a computer then numbers are about as efficient as it gets for storage
  * ++ computers are faster at grouping and processing numbers than text in most cases
  * -- storing as numbers may encourage you to do things to the data which wouldn't make sense i.e. the numbers are "efficient flags" rather than representing a quantity or cardinality
      * if you ask humans to rate something 1-5 you can't assume they will unbiased so you cannot infer that the difference in the measured value between "1" and "2" and the difference between "2" and "3" is equal even though the numerical representation encourages you to think it is. If we rated films as "hate", "dislike", "meh", "like", "love" then we aren't as tempted to have that sloppy thinking.
    * Idea: it may be best to think of and discuss the ratings as english words with the other humans involved and keep the numerical representation (aka "efficient digital flags") as a hidden implementation detail so the team doesn't get distracted by the numbers.
  * ++ it lets us plot the values on graphs and manipulate them with math functions
      * WARNING: while the math can be correct, we need to remember that our numbers have assumptions built into them.

Ideas for storing preferences as numerical values:

* rating 1 to 5
* 0 = did not purchase, 1 = purchased
* -1 = disliked, 0 = did not vote, 1 = liked

## Metrics (distance functions)

> In mathematics, a metric or distance function is a function that defines a distance between each pair of elements of a set.
>
> https://en.wikipedia.org/wiki/Metric_(mathematics)

Examples of metrics are:

* Hamming distance (in coding)
* Levenshtein distance (over strings)

Some common ways of calculating "distance" between two elements in a set of data:

1. Euclidean distance
1. Pearson correlation
1. Jaquard index
1. Manhattan (or Taxicab) distance

Calculating metrics in code:

* Ruby
    * https://github.com/agarie/measurable
* python
    * scipy has functions for all these (or so it seems to me)
    * https://docs.scipy.org/doc/numpy/reference/generated/numpy.linalg.norm.html
* R
    * dist()


Tip: Inject metric functions as a dependency

Making the functions which implement these various metrics have the same signature is handy because it lets you try out different metrics with your data set(s). You might have to do some massaging of the output number to make them all comparable.

```
// @returns a float between 0 <-> 1 or -1 <-> 1 where we interpret 1 to mean a perfect match
fn some_metric(a: Vector, b: Vector) -> Float {
    // ...
}
```

### Metric: Euclidean distance

Consider our list of critics, each of which has given a ranking from 1 to 5 for a number of movies. All critics have not seen all the same movies.

Take the items that people have ranked in common and plot them on a graph which lets us visualise which people are "closer together"

#### Consider a 2D example

Pick two movies and use them as the axes of a graph and then graph the position of each critic who has reviewed both movies.

Then calculate the _euclidean distance_ between each critic - the smaller the distance the closer the critics tastes are.

Consider two points on the graph $x_1, y_1$ and $x_2, y_2$

we can calculate euclidean distance $d$

$$
d = \sqrt{(x_1 - x_2)^2 + (y_1 - y_2)^2}
$$

Notice that this equation is basically pythageorus' theorem.

The euclidean distance $d$ has the following properties:

* $d$ can take on any value from $0 \rightarrow \infty$
* $d$ is smaller for those who ratings are more similar

We want our rating $r$ to be

1. between 0 and 1
2. be bigger the more similar people's ratings are

The value we want is the output of $\frac{1}{x}$ where $x$ can have values $1 \rightarrow \infty$ - see https://www.wolframalpha.com/input/?i=plot+1%2Fx+where+x+from+0+to+5

$$
r = \frac{1}{1+d} = \frac{1}{1+\sqrt{(x_1 - x_2)^2 + (y_1 - y_2)^2}}
$$

We can ensure that $d$ is never less than 1 by adding 1 to it. We can get away with this fiddle because we don't care about the absolute values of the output - we only care about their values relative to each other (adding 1 to both sides of an inequality doesn't change it).


#### Aside: euclidian distance in many dimensions

The euclidian distance calculus is independent of the number of dimensions.

The euclidian distance between vectors $a$ and $b$ where $a$ and $b$ are $n$ dimensional vectors can be written as:

$$
dist(a,b) = \sqrt{\sum_{i=1}^{n} (a_i - b_i)^2}
$$

Or, more specifically
For example if $n=4$ dimensions:

$$
dist(a,b) = \sqrt{ (a_1-b_1)^2 + (a_2-b_2)^2 + (a_3-b_3)^2 + (a_4-b_4)^2 }
$$

```python
# $ ipython -pylab

from scipy import spatial
a = [1,1,1,1]
b = [2,2,2,2]
spatial.distance.euclidean(a, b)
# 2.0
```

### Metric: Pearson correlation coefficient $\rho$

* also called _population correlation coefficient_

> Pearson's correlation coefficient is the covariance of the two variables divided by the product of their standard deviations. The form of the definition involves a "product moment", that is, the mean (the first moment about the origin) of the product of the mean-adjusted random variables; hence the modifier product-moment in the name.

https://en.wikipedia.org/wiki/Pearson_product-moment_correlation_coefficient

$$
\rho = \frac{cov(X, Y)}{\sigma_X\sigma_Y}
$$

where

* $cov(X,Y)$ is the covariance
* $\sigma_X$ is the standard deviation of $X$

Pearson correlation coefficient

* is a measure of how well two sets of data sit on a straight line
* better than euclidean distance when the data is not normalized because it corrects for **grade inflation** e.g. some individuals might use 1 for products they dislike while others might use 3.
    * Note there may be cases where not correcting for grade inflation is what you want

### Metric: Jaccard coefficient

* https://en.wikipedia.org/wiki/Jaccard_index
* The Jaccard **coefficient** measures **similarity** between finite sample sets, and is defined as the size of the intersection divided by the size of the union of the sample sets
* The Jaccard **distance**, which measures **dissimilarity** between sample sets, is complementary to the Jaccard coefficient and is obtained by subtracting the Jaccard coefficient from 1
* Seems to be very popular in recommendation systems

Terminology aside:

$$
A = \text{The set A}\\
|A| = \text{The number of elements in A}
$$

The jaccard coefficient $J$:

$$
J(A, B) = \frac{|A \cap B|}{|A \cup B|}
$$

The jaccard distance $d_J$:
$$
d_J(A, B) = 1 - J(A, B)
$$

### Metric: Manhattan distance

* also called _Taxicab distance_
* represents the path a taxi would take from one point to another in a city with regular square blocks such as Manhattan)
* https://en.wikipedia.org/wiki/Taxicab_geometry
