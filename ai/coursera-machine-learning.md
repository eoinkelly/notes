# Coursera Machine Learning Course

_Aside: As of 2016-12-23 Github does not render Math correctly so you will need to use a multi-markdown capable renderer_

## Week 1

* ML grew out of AI
* ML touches many areas
    * database mining: click-stream data, medical data, computational biology, all fields of engineering
    * Applications we can't program by hand
        * autonomous helicopters
        * handwriting recognition (used to route real world mail)
        * Most Natural language processing (NLP) is applied ML
        * Most Computer Vision is applied ML
    * Self customizing programs
        * product recommendations from Amazon, Netflix
    * Understanding human learning and the brain and real AI

> Two definitions of Machine Learning are offered. Arthur Samuel described it
> as: "the field of study that gives computers the ability to learn without
> being explicitly programmed." This is an older, informal definition.
>
> Tom Mitchell provides a more modern definition: "A computer program is said
> to learn from experience E with respect to some class of tasks T and
> performance measure P, if its performance at tasks in T, as measured by P,
> improves with experience E."
>
> Example: playing checkers.
>
> E = the experience of playing many games of checkers
>
> T = the task of playing checkers.
>
> P = the probability that the program will win the next game.

There are two broad categories of ML

1. Supervised learning
1. Unsupervised learning

## Supervised learning

* most common type of ML problem
* the algorithm is given the correct answer for each data point in the training data set
* we have a pre-concieved idea that there is a relationship between the data and we train the algorithm on those past results hoping it will be able to predict new outputs given new inputs
* the task of the algorithm is to produce new answers based on new points which were not in the training data.

> In supervised learning, we are given a data set and already know what our
> correct output should look like, having the idea that there is a relationship
> between the input and the output
>
> Supervised learning problems are categorized into "regression" and
> "classification" problems. In a regression problem, we are trying to predict
> results within a continuous output, meaning that we are trying to map input
> variables to some continuous function. In a classification problem, we are
> instead trying to predict results in a discrete output. In other words, we
> are trying to map input variables into discrete categories.
>
> Example 1:
>
> Given data about the size of houses on the real estate market, try to predict
> their price. Price as a function of size is a continuous output, so this is a
> regression problem.
>
> We could turn this example into a classification problem by instead making
> our output about whether the house "sells for more or less than the asking
> price." Here we are classifying the houses based on price into two discrete
> categories.
>
> Example 2:
>
> (a) Regression - Given a picture of a person, we have to predict their age on
> the basis of the given picture
>
> (b) Classification - Given a patient with a tumor, we have to predict whether
> the tumor is malignant or benign.

Supervised learning  problems are categorized into "classification problem" or "regression problem"

1. classification problem
    * output is a discrete function of input i.e. it maps the inputs onto a discrete set of values
    * we want an algorithm which will predict discrete valued output
    * example: predicting whether tumor is malignant or benign (two possible output values which are not continuous)
1. regression problem
    * output is a continuous function of input
    * we want an algorithm which will predict continuous valued output
    * example: house prices

Given a real world problem you can usually ask questions about it which fall into either category.

A good learning algorithm is one which can use up to an infinite no. of features of the problem to make predictions. There are algorithms which can use many features to make predictions e.g. support vector machine - see https://en.wikipedia.org/wiki/Support_vector_machine. SVMs use a thing called the "kernel trick" to use many features in a computationally efficient way.

## Unsupervised learning

We give the algorithm a data set and ask it to find some structure in it

Examples of unsupervised algorithms

1. clustering algorithms
    * categorizing the data into groups based on some criteria
    * Examples of clustering
        * http://news.google.com is an example of a clustering algorithm
        * Trying to figure out which machines send traffic to each other in large data centers so you can optimize your network
        * Social network analysis - can you identify groups of friends based on their contact with each other
        * Market segmentation - have an algorithm create market segments for you based on your customers
        * Astronomical data analysis
1. cocktail party algorithm
    > The cocktail party effect is the ability to focus on a specific human voice while filtering out other voices or background noise. The ease with which humans perform this trick belies the challenge that scientists and engineers have faced in reproducing it synthetically.

> Unsupervised Learning
>
> Unsupervised learning allows us to approach problems with little or no idea
> what our results should look like. We can derive structure from data where we
> don't necessarily know the effect of the variables.
>
> We can derive this structure by clustering the data based on relationships
> among the variables in the data.
>
> With unsupervised learning there is no feedback based on the prediction
> results.
>
> Example:
>
> Clustering: Take a collection of 1,000,000 different genes, and find a way to
> automatically group these genes into groups that are somehow similar or
> related by different variables, such as lifespan, location, roles, and so on.
>
> Non-clustering: The "Cocktail Party Algorithm", allows you to find structure
> in a chaotic environment. (i.e. identifying individual voices and music from
> a mesh of sounds at a cocktail party).

### Gnu Octave

* Free implementation of Matlab (mostly compatible with Matlab scripts)
* This course uses the "Octave" programming environment to implement algorithms
* A common pattern is to prototype the algorithm in Octave and then implement in C++/Java later on

Their reasoning for not using R is:

> R is a bit too high level. This course shows how to actually implement the
> algorithms of machine learning, while R already has them implemented

Aside: => R might be practical for me using these outside of this course.  

## Our first (supervised) learning algorithm : Linear regression with one variable

### 1. Categorize the Problem

Predict the price a particular house will sell for given a training data set of price vs size-in-feet.

This is an example of

* supervised learning problem
* regression (continuous valued output) problem

> When the target variable that we’re trying to predict is continuous, such as
> in our housing example, we call the learning problem a regression problem.
> When y can take on only a small number of discrete values (such as if, given
> the living area, we wanted to predict if a dwelling is a house or an
> apartment, say), we call it a classification problem.

### 2. Human choose a model & machine find the best parameters for it

Given a set of data points which historical data, we need to choose an equation shape (model) which we think will fit the data well enough to provide good predictions for the future.

Once we have that we can use ML to learn the best parameters for the equation that will make it fit the best with the data we have.

In other words:

1. Human: Choose an equation which will match the data pretty well (the model)
1. Human: Choose a cost function which expresses when parameters of that equation are better/worse fit for the existing data1
1. Human: Choose an algorithm which will minimize the cost function
1. Machine: Run the minimization function and find the best parameters.

The week's material does not mention anything about **how** you choose this equation (model).

The learning algorithm's output is a *function* called $h$ or the _hypothesis_ - the _hypothesis_ is our chosen model (equation) with the best possible parameters filled in by the learning algorithm.

```hs
# in haskell type terminology
learning_algorithm :: training_data_set -> h
h :: x -> predicted_y
```

### 3. Choose a model for the data

* $X$ = the space of the input variable or "input feature"
* $Y$ = the space of output values or "target feature"
* $X = Y = \mathbb{R}$ in this case
* $x^{(i)}$ is the $i^{th}$ input example (superscript $i$ is not exponentiation here)
* $y^{(i)}$ is the corresponding actual output value (not an estimate) from the training set.
* So $(x^{(i)}, y^{(i)})$ is a "training example" and $(x^{(i)}, y^{(i)});i = 1...m$ is the training set (a set of examples).
* There are $m$ training examples in the set.

We are trying to learn a function $h(X) \rightarrow Y$ so that $h(x)$ is a **good predictor** of $y$

$h(x)$ is a the "predicted value of y"


How we build a model

A model is our guess at what the relationship between the data is - it may not actually _be_ the relationship between the data!

To begin we (humans) are going to create a simple model and assume that $h$ will be a _linear function_ i.e.

$$h_\theta(x) = \theta_0 + \theta_1x$$

sometimes written as

$$h(x) = \theta_0 + \theta_1x$$

We created this model but we want the machine to fill in the values of $\theta_0$ and $\theta_1$ for us so this is really a combination of machine learning to tweak the blueprint set down by human. This is a form of _parameter learning_.

$h$ is linear because if you were to plot $a + bx$ you would get a straight line

This model is called **linear regression with one variable** or **univariate linear regression**

* linear = the model is always a line
* regression = the model has continuous valued output
* univariate = the model depends on one variable only ($x$)
    * Note that $\theta_0$ and $\theta_1$ are **constants** in the model

Given a training dataset of m examples we want to build a function which will
take an input value (from $X$) and and return a predicted $Y$ value.

### 4. Define a cost function

Now that we have decided on a model we need to create some way of finding $\theta_0$ and $\theta_1$. We need two things for this

1. A way of measuring how "good" the model's predictions are for a particular $\theta_0,\theta_1$
    * This is the cost function
2. A way of walking through the $\theta_0, \theta_1$ space and finding the values of $\theta_0, \theta_1$ for which the prediction is the best (or actually least worst)
    * This is the cost minimizing function

We start by defining a function which will express how much error there is between the predicted output and the actual output for a particular example from the training set.

We want a function whose output

* has a value of 0 if there is no error
* goes up as the error goes up
* returns proportionally larger values as error goes up (big errors are penalized more)
* does not care whether the predicted output was too low or too high

Our Cost function

$$
J(\theta_0,\theta_1) = \frac{1}{2m}
\sum_{i=1}^{m}
\bigg(
  h_\theta(x^{(i)}) - y^{(i)}
\bigg)^2
$$

which can be written as

$$
J(\theta_0,\theta_1) =
\frac{1}{2}
\frac{1}{m}
\sum_{i=1}^{m}
\bigg(
  \text{diff between prediction and actual}
\bigg)^2
$$

$$
J(\theta_0,\theta_1) =
\frac{1}{2}
\text{arithmetic mean}
\bigg(
  (
  \text{diff between prediction and actual}
  )^2
\bigg)
$$

The output of our cost function goes up with the square of the diff between actual and predicted i.e. as errors get bigger the cost goes up with the square

> To break it apart, it is $\frac{1}{2}\bar{x}$ where $\bar{x}$ is the mean of the squares of $h_\theta(x^i)−y^i$ , or the difference between the predicted value and the actual value.
>
> This function is otherwise called the "Squared error function", or "Mean squared error".
>
> The mean is halved ($\frac{1}{2}$) as a convenience for the computation of the gradient descent, as the derivative term of the square function will cancel out the $\frac{1}{2}$ term.

### 5. Choose algorithm to minimize cost function

#### Gradient Descent algorithm

Gradient descent can find a *local* minmum

$$h_{\theta}(x) = \theta_0 + \theta_1x$$

> The way we do this is by taking the derivative (the tangential line to a function) of our cost function.
>
> The slope of the tangent is the derivative at that point and it will give us a direction to move towards.
>
> We make steps down the cost function in the direction with the steepest descent.
>
> The size of each step is determined by the parameter α, which is called the learning rate.
used to minimize some arbitrary function J
in our case J will be the cost function of a particular hypothesis



$$
\alpha = \text{The learning rate (the size of step to take each time)}
$$

$$\text{Repeat until convergence}$$
$$
\theta_j =
\theta_j -
\alpha
\frac{\partial}{\partial \theta_0}
J(\theta_0, \theta_1)
$$

Or in more detail (showing the simultaneous updating of $\theta_0$ and $\theta_1$

$$
temp0 =
\theta_0 -
\alpha
\frac{\partial}{\partial \theta_0}
J(\theta_0, \theta_1)
$$

$$
temp1 =
\theta_1 -
\alpha
\frac{\partial}{\partial \theta_1}
J(\theta_0, \theta_1)
$$

$$
\theta_0 := temp0
$$

$$
\theta_1 := temp1
$$

* Note that $\partial$ (the partial derivative) is not the same as $\delta$ (the full/total derivative). $\partial$ indicates that you are differentiating wrt to only one of the variables while holding the other constant.
* note that the slope of the tangent to our current point in one direction (a.k.a. the partial derivative) can be positive or negative so even though we always substract the derivative term we could actually be increasing $\theta_j$

In english, gradient descent is

1. start with some a, b
1. keep changing a, b to reduce J(a, b) until we hopfully end up at a minimum

if you imagine a plot of the surface J(a, b) where a = x axis, b = y axis, J(a,b) = z axis
then imagine yourself standing on that surface and looking around 360 degrees to find the step you can take which will descend you the most


The algorithm covered here is actually _batch_ gradient descent algorithm.
_Batch_ refers to that the algorithms looks at the **entire training set** (the sum term is over all m).

> This method looks at every example in the entire training set on every step, and is called batch gradient descent.
>
> Note that, while gradient descent can be susceptible to local minima in general, the optimization problem we have posed here for linear regression has only one global, and no other local, optima; thus gradient descent always converges (assuming the learning rate α is not too large) to the global minimum.
>
> Indeed, J is a convex quadratic function.

There is a another methods of minimizing $J$ called the "normal equations" method that just solves for the minimum but it doesn't scale as well as gradient descent.

Q: scale in what way? computationally?

### 6. Evaluate our results

* Hopefully our gradient descent algorithm has converged
* So now we have values for $\theta_0$ and $\theta_1$ which provide the best possible fit for our (human) chosen model to the data.

Q: can we have machines choose the model too?

Q: How do we know its a global minimum not a local minimum?

### Matrix Algebra Refresher

Terminology

* $\mathbb{R}^{4 \times 2}$ is the set of all 4x2 matrices
* $\mathbb{R}^{3 \times 2}$ is the set of all 3x2 matrices
* $A_{ij}$ - element from row $i$, column $j$ of the matrix $A$
* Matrix names are usually capital letters

A vector is a matrix with only one **column**:

$$y = \begin{bmatrix}1 \\ 33 \\ 4 \\ 6\end{bmatrix}$$

* Vectors are often represented by an array in code _but_ we should remember that a vector, unlike how an array is presented in code, is a single *column* not a single *row*.
* Math usually begins vector and matrix indexing at 1 not 0

#### Matrix Addition

$$
\begin{bmatrix}a & b\\c & d\end{bmatrix} + \begin{bmatrix}e & f\\g & h\end{bmatrix} = \begin{bmatrix}a+e & b+f\\c+g & d+h\end{bmatrix}
$$

* you can only add matrices with the same dimensions

#### Matrix Subtraction

$$
\begin{bmatrix}a & b\\c & d\end{bmatrix} -
\begin{bmatrix}e & f\\g & h\end{bmatrix} =
\begin{bmatrix}a-e & b-f\\c-g & d-h\end{bmatrix}
$$

* you can only subtract matrices with the same dimensions

#### Multiplying (by a scalar)

$$
3 *
\begin{bmatrix}a & b\\c & d\end{bmatrix}
=
\begin{bmatrix}3a & 3b\\3c & 3d\end{bmatrix}
$$

#### Division (by a scalar)

$$
4 \div
\begin{bmatrix}a & b\\c & d\end{bmatrix}
=
\frac{1}{4} *
\begin{bmatrix}a & b\\c & d\end{bmatrix}
=
\begin{bmatrix}\frac{a}{4} & \frac{b}{4}\\\frac{c}{4} & \frac{d}{4}\end{bmatrix}
$$

* scalar division is really scalar multiplication by the inverse of the scalar

#### Vector by Matrix multiplication

$$
\begin{bmatrix}a & b\\c & d\\e & f\end{bmatrix}
.
\begin{bmatrix}g\\h\end{bmatrix}
=
\begin{bmatrix}ag + bh\\cg + dh\\eg + fh\end{bmatrix}
$$
$$
(3 x 2) . (2 x 1) = (3 . 1)
$$
$$
(X x Y) . (Y x Z) = (X x Z)
$$

To multiply a matrix and a vector the inner dimensions must be the same

Example problem: Given $h_{\theta}(x) = -40 + 0.25x$ and a collection of house prices:

$$
\begin{bmatrix}2104\\1416\\1534\\852\end{bmatrix}
$$

we want to compute $h_\theta(x)$ for all values in the training set.

We can use matrix by vector multiplication to do this in a cleaner way that iterating over the data set in code. It is also faster computationally (according to instructor - is this true?)

$$
\begin{bmatrix}1 & 2104\\1 & 1416\\1 & 1534\\1 & 852\end{bmatrix}
*
\begin{bmatrix}-40\\0.25\end{bmatrix}
=
\begin{bmatrix}?\\?\end{bmatrix}
$$

$$
(4 x 2) . (2 x 1) = (4 x 1)
$$

$$
\begin{bmatrix}1 & 2104\\1 & 1416\\1 & 1534\\1 & 852\end{bmatrix}
*
\begin{bmatrix}-40\\0.25\end{bmatrix}
=
\begin{bmatrix}
-40 * 1 + 0.25 * 2104\\
-40 * 1 + 0.25 * 1416\\
-40 * 1 + 0.25 * 1534\\
-40 * 1 + 0.25 * 852\\
\end{bmatrix}
=
\begin{bmatrix}
486\\
314\\
343.5\\
173\\
\end{bmatrix}
$$

```ruby
# doing the calculation in ruby

# ruby matrices are immutable
require "matrix"

v = Matrix.column_vector([-40, 0.25])
# or
# v = Matrix[[-40], [0.25]]

m = Matrix[[1, 2104], [1, 1416], [1, 1534], [1, 852]]
m * v
# => Matrix[[486.0], [314.0], [343.5], [173.0]]
```

#### Matrix Matrix multiplication

The inner dimensions have to match:

$$ 2x3 \times 3x2 = 2x2$$

Then split the second matrix into vector columns and multiply the first matrix by each column to create a column in the answer matrix

Given

$$
\begin{bmatrix}a & b & c\\d & e & f\end{bmatrix}
\times
\begin{bmatrix}u & v\\w & x\\y & z\end{bmatrix}
$$

First we take the first vector column and multiply it out

$$
\begin{bmatrix}a & b & c\\d & e & f\end{bmatrix}
\times
\begin{bmatrix}u\\w\\y\end{bmatrix}
=
\begin{bmatrix}
au + bw + cy \\
du + ew + fy \\
\end{bmatrix}
$$

Then take the second vector column

$$
\begin{bmatrix}a & b & c\\d & e & f\end{bmatrix}
\times
\begin{bmatrix}v\\x\\z\end{bmatrix}
=
\begin{bmatrix}
av + bx + cz\\
dv + ex + fz\\
\end{bmatrix}
$$

and putting it all together:

$$
\begin{bmatrix}a & b & c\\d & e & f\end{bmatrix}
\times
\begin{bmatrix}u & v\\w & x\\y & z\end{bmatrix}
=
\begin{bmatrix}
au + bw + cy & av + bx + cz\\
du + ew + fy & dv + ex + fz\\
\end{bmatrix}
$$

In general

* you can only multiply matrices if their dimensions are compatible e.g. $m x n \times n x o$
* when multiplying matrices $A \times B = C$, the $i_{th}$ column of matrix $C$ is obtained by multiplying matrix $A$ by the $i_{th}$ column of matrix $B$


#### Matrix multiplication is not commutative

Scalar mutiplication is commutative, matrix multiplication is not. $A \times B \neq B \times A$

Remember you can only multiply matrices if their dimensions are compatible i.e. $m x n \times n x o$ works but $n x o \times m x n$ does not.

#### Matrix multiplication is associative

Scalar multiplication is associative and so is matrix multiplication (it doesn't matter how you group the terms in a multiplication)

$$
A \times B \times C
$$
$$
(A \times B) \times C = A \times (B \times C)
$$

#### Identity matrix

* In scalar multiplication $1$ is the identity operation
* in matrix multiplication the identity matrix is denoted $I_{n \times n}$ (there is a different identity matrix for each size of matrix)
    * identity matrix is _always_ square
    * it has 1's on the left to right diagonal and 0's everywhere else

$$
\begin{bmatrix}1\end{bmatrix}
$$
$$
\begin{bmatrix}1 & 0\\0 & 1\end{bmatrix}
$$
$$
\begin{bmatrix}1 & 0 & 0\\0 & 1 & 0\\0 & 0 & 1\end{bmatrix}
$$
$$
A \times I = I \times A = A
$$
$$
\text{note that I's above have different dimensions}
$$

#### Matrix inverse

$1$ is the identity in scalar multiplication which implies that for each number x there exists some "inverse" number $x^{-1}$ such that $x \times x^{-1} = 1$

Not all numbers have an inverse e.g. 0

For matrices the inverse is that for a matrix $A$ there exists another matrix $A^{-1}$ for which

$$
A . A^{-1} = I
$$

is true.


$$
A_{n \times n} \times A_{n \times n}^{-1} = I
$$

Only square matrices have inverses (because the identity matrix is always square) but **not all square matrices have inverse** e.g. this matrix has no inverse:

$$
\begin{bmatrix}0 & 0\\0 & 0\end{bmatrix}
$$

#### Matrix transpose

To get transpose of A, take each row of A and turn it to a column of $A^T$

If $A_{m \times n}$ then $A^T_{n \times m}$
$$
A = \begin{bmatrix}a & b & c\\d & e & f\end{bmatrix}
$$
$$
A^T = \begin{bmatrix}a & d\\b & e\\c & f\end{bmatrix}
$$

## Week 2
