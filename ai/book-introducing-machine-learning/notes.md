# 1. Introducing Machine Learning

- [1. Introducing Machine Learning](#1-introducing-machine-learning)
  - [1.1. Overview](#11-overview)
  - [1.2. Jargon](#12-jargon)
  - [1.3. A short history of our attempts to systematize human thought](#13-a-short-history-of-our-attempts-to-systematize-human-thought)
  - [1.4. Define _intelligence_](#14-define-intelligence)
  - [1.5. Artificial forms of intelligence](#15-artificial-forms-of-intelligence)
  - [1.6. The data scientist](#16-the-data-scientist)
  - [1.7. A Taxonomy of ML problems and solutions](#17-a-taxonomy-of-ml-problems-and-solutions)
    - [ML Problems](#ml-problems)
      - [1. Shallow Learning Problems](#1-shallow-learning-problems)
      - [2. Deep Learning Problems](#2-deep-learning-problems)
  - [Automated Machine Learning (AutoML)](#automated-machine-learning-automl)
    - [An example manual ML Workflow](#an-example-manual-ml-workflow)
- [2. Chap 16](#2-chap-16)
  - [2.1. Neural Networks](#21-neural-networks)

## 1.1. Overview

AI is

-   attempts to build a set of _cognitive services_
-   about a lot of trial and error (this is very important)
-   ML is just a new speciality in software similar to web development or database development
-   They describe it as a _breakthrough_ because it allows us to do things which were previously not possible

Building a ML workflow requires:

1. data transformations
2. training algorithms
3. evaluation metrics
4. domain knowledge
5. knowledge base
6. trial-and-error attitude
7. imagination

## 1.2. Jargon

-   Algorithm
    -   generally refers to a whole family of algorithms which share the same general approach
-   Trainer or Estimator
    -   A specific implementation of an algorithm
-   Data transformation
    -   Transforming your data sets to make them suitable for use in training and evaluation
-   Evaluator
    -   The process of evaluating how well the model performs. Usually you split your input dataset into training data and test data (perhaps 90% training, 10% test) then you can run the test data (which the model has not seen before) through the model and see how good it's output is.
-   ML Pipeline
    -   A process diagram of the full solution
    -   the overall combination of data transformations, trainers, and evaluators that form the ultimately deployed machine learning model.
    -   Sometimes called: _Learning pipeline_, _ML pipeline_
-   AutoML
    -   Automatic selection of the best ML pipeline for a given problem _and_ a given dataset
    -   Obviously convenient for less experienced folks
    -   Obviously limited
-   Logistic function
    -   A logistic function is one that grows or decays rapidly for a period of time and then levels out.
    -   A curve that grows exponentially at first and then slows down and hardly grows at all
    -   Good for modelling exponential growth in a situation where some other factor kicks in to limit it
    -   Examples:
        -   human population growth in an area is eventually limited by lack of food and resources
        -   bacteria growth
        -   spread of disease
    -   use it in a model where you see:
        1. initial growth is exponential
        2. growth eventually levels off
    -   formula:
        -   $c = \text{carrying capacity}$ (the limit to the growth)
        -   $c/2 = \text{the value of L where growth is fastest}$
        -   $a, b = \text{constants}$ (you vary these to tweak the curve)
            $$f(x) = \frac{c}{1 + a \cdot b^x}$$
    -   formula (expressed in population terms using $e$ function):
        -   $t = \text{time}$
        -   $P(t) = \text{population at time t}$
        -   $L = \text{population limit}$
        -   $L/2 = \text{The point where growth is increasing the fastest}$
        -   $K = \text{continuous growth rate (only for small t)}$
        -   $C = \text{a constant}$
            $$P(t) = \frac{L}{1 + C \cdot e^{-Kt}}$$
-   Regression
    -   builds a model which can discover a cause -> effect relationship between input and output data.
    -   measures the strength of the relationship set between one output variable and a number of input variables
    -   is a _supervised_ technique
    -   is used to predict continuous values (as opposed to discrete values as categorization does)
    -   allows it to forecast a value for a new input based on that model
    -   there are many regression algorithms
        -   they differ in
            -   number of input variables they account for
            -   type of relationship between input values and known output values

## 1.3. A short history of our attempts to systematize human thought

-   Euclid (circa 300 BCE)
    -   wrote down 5 axioms of geometry
    -   derived all his geometric theories from those axioms
    -   an example of _formal reasoning_
-   Leibniz conjectured that human thought could be systemized into a set of algebraeic rules so that reasoning could be reduced to a mechanical activity
    -   Being mathematicians, the kind of human thought they were talking about most is mathematical reasoning
-   1900 Hilbert set a goal for mathetmatics:
    -   _Can all mathetmatical statements be expressed and manipulated through a set of well-defined rules?_
        -   He wanted to formalize all mathematical reasoning
        -   He wanted to emulate Euclid and find a set of axioms from which all mathematics could be derived
-   1931 Kurt Godel's _incompleteness theory_ says that Hilberts wish wasn't possible

    -   > In any formal system expressive enough to model the arithmetic of natural
        > numbers, there is at least one undecidable statement that evidence proves
        > true but that can’t be proven true or false within the axioms of the
        > system.
    -   Godel's theory is a sort of good news/bad news for AI
        -   Godel's theory says there are lines beyond which mathematical logic cannot go
        -   Godel's theory says that, provided you stay within the limits of a formal system, any reasoning can be expressed as a set of transformation rules so can be mechanised
            -   This implication builds the foundations for mechanical (aka computer) reasoning

-   Three classes of computable functions were defined in the 1930's:
    1. 1933: Godel formulates the idea of _general recursive functions_ (a computable function which takes a finite array of natural numbers and returns a natural number
    2. 1936: Alonzo Church defines a formalism called _lambda calculus_ to express similar computations on natural numbers
    3. 1936: Alan turing built the theoretical model of a computing machine to perform calculations in terms of symbols written on infinite tape
-   Later the _Church Turing thesis_ unified all three classes proving that a function is computable in the lambda calculus iff it is computable in the turing machine iff it is a general recursive function
    -   The outcome of this thesis is that it became plausible that you could build a machine which would be able to reproduce any plausible process of mechanical deduction through the manipulation of symbols
    -   All the early computers (ENIAC, Colussus, Bombe, Lorentz machine) were based on this foundation
    -   IMPORTANT: It is important because it shows that computers can do more than just calculating numbers from other numbers - they can calculate symbols from symbols (assuming the symbols are expressed in a consistent grammar)
-   1943: McCullogh and Pitt propose a computational model inspired by the known structure of the brain's neuron
    -   This work is the foundation of neural networks
    -   This work pre-dated actual computing machines so we could (in an alternate timeline) have had computers based on neuorons from the start
    -   => AI has been around as long as computers have
-   This "symbol calculation" may not be the same as "thinking"
    -   e.g. you could pass a turing test in a language you don't speak with a good enough dictionary but that doesn't mean you are _thinking_ in that language
    -   Quote:
        > You’re in the 1950s and you know you can build machines to compute
        > anything you can express through a consistent grammar of symbols, not just
        > calculating numbers from numbers
-   AI was officially created in 1956
    -   John McCarthy organised a summer research workshop and invited experts in different fields to work on creating an artifical brain
    -   He coined the term _artificial intelligence_
    -   He wanted to unify two paths of research which were in progress
        1. Automata theory (descended from Church & Turing)
        2. Cybernetics (descended from Babbage and Von Neuman)

## 1.4. Define _intelligence_

-   Intelligence is also the ability to
    1. Form judgment and opinions out of the acquired knowledge
    2. Act based on that
    3. React to unknown events
-   Alternatively

    -   A combination of cognitive abilities
        1. Perception
        2. Memory
        3. Language
        4. Reasoning
    -   A specific learning approach to
        1. extract information
        2. transform information
        3. store information

-   Normal software has it's reaction to situations hard-coded in to it
    -   Anything smart it does is because that behaviour was coded into it
    -   It will not get any better at the job over time unless the coding is changed
-   "Intelligent" software can get better at a task without having to be re-programmed
    -   Intelligent software can give different answers to the same question at different times if the conditions are different
    -   There seems a pretty fine line between "reprogram" and "retrain" ???
    -   ?? Are there situations where an ML algorithm can train as it predicts or are those steps always separate?
        -   ?? Assuming the algorithm has some way of finding out whether its predictions were good/bad this is quite feasible?

Neurons

-   Humans (90 billion neurons )have fewer neurons that many animals (Elephants = 250 billion neurons)
-   Theory is that intelligence goes with number of neurons in the cerebral cortex
-   We have 10^16 synapses
-   Or maybe in the hills and valleys of the brain - our brains are wrinkly, mice are smooth
    -   Elephants and dolphins have more wrinkly brains than us
        -   => they might be more intelligent than us but their bodies (lack of vocal cords) limit the expressiveness of it

"CPU stats" of the Human brain

-   Human brain clock speed is approx 100Hz
-   Estimates of energy required to run the brain is 12W
-   10^16 synapses firing at 100 times/sec = 10^18 operations per second
-   Human memories are quite different to computers
    -   Humans rebuild a memory every time it is accessed

An artificial intelligence needs to be able to

1. infer information in a number of ways
2. process it
3. retain it so that it can be uses to process new inputs

## 1.5. Artificial forms of intelligence

Kinds of artifical intelligence available today:

1. "if statements"
    - a very basic form of intelligence
    - no learning (no memory of past decisions or way to change new decisions based on them)
2. Expert systems (_Applied AI_)

    - can be thought of as "if based rules"
    - strongly based on inherited work from the past
    - encodes the knowledge of a human expert
    - examples
        - aircraft auto-pilot
        - software for reading x-rays
    - An expert system is made up of two subsystems:
        1. a fixed, hard-coded database of known facts
        2. an inference engine
    - each input is processed by running the set of known facts through the inference engine
    - good when a human expert would be too expensive or too hard to find
    - a good expert system can give the same answers a human expert would when asked questions about a very narrow field
        - unlike humans, expert systems are not able to learn
        - like humans, expert systems are expensive to gain the expertise in the first place and also expensive to maintain the expertise
    - expert systems were the first truly successful form of AI
    - ++ good within it's narrow well-defined range
    - -- bad at anything else. Cannot handle a large range of contextual data
    - -- new answers are only possible by shipping a new version of the software
    - expert systems don't do any learning on the job
    - Example

        ```
        # facts database
        If X < 14 years => X loves pets
        If X > 14 years => X loves jewelry
        If X loves pets => X gets a puppy dog
        If X loves jewelry => X gets a necklace
        If X gets a necklace and X is 18+ => X gets a gold necklace

        # inference engine
        while (exists a rule to apply to current knowledge){
            select rule
            execute rule
            update knowledge
        }
        ```

    - ?? How are expect systems different from nested if-else?
        - You could make a rule based system with nested if-else but there would be a **lot** of duplication and extra complexity
        - They allow for much more complex heirarchies than you could easily make

3. Autonomous systems (_General AI_ or _Machine Learning_)

    - designed to work as expert systems on data they have not seen before
    - lays the groundwork for the future
    - Instead of hard-coding a long list of cases to recognise, a short list of _patterns in the inputs_ are hard-coded
        - Q: what does that mean?
    - The way the software reacts is based on the huge quantity of data used to train it
    - -- the primary limitation of autonomous systems is the quality of the data
    - examples
        - fraud detection systems
        - recommendation engines
        - natural language processing
        - power production forecasts in energy
    - are not necessarily better than expert systems
    - are divided into two major kinds based on the kind of learning they receive:

        1. Unsupervised learning (learning by discovery)
            - a set of algrorithms which identify subsets of data which have something in common
            - very close to data mining
            - they all work on "unpolished" data and try to find clusters (or commonalities) within it
            - ideally the algorithm suggests correlations which lead to further analysis
            - representing a table as a graph is an attempt to let human brains do this "identifying of subsets"
            - each algorithm has a way to measure the _logical distance_ between rows in the data set
                - if you only had two features in your data set you could plot them on a graph and then measure the "physical distance" between them to group them (physical distance on a graph is also just a formula which calculates a number which represents the distance)
                    - this is what we do when we graph data
                - when you have more than 3 features, graphing doesn't make sense anymore but we can still talk about the "distance" between rows
            - the resulting groups can be
                - a partition (all data items are in exactly one subset)
                - have overlap (some data items appear in multiple subsets)
                - have some data items which are not in any subset (anomalies)
            - dimensionality reduction
                - steps
                    1. replace either an entire subset or part of a subset with a single representative data item
                    2. feed the resulting data set into your unsupervised learning algorithm again
                    3. rinse, repeat (AI is a lot of trial and error)
                - examples
                    - replace all data items in a cluster with a single data item which represents the mean of **all values**
        2. supervised learning (learning by example)
            - given both an input dataset and an output dataset, supervised learning tries to find the path between the two so that it can guess the output for unseen inputs
            - contains a whole class of algorithms
            - has 2 goals
                1. Prediction
                    - Relies heavily on regression and is good at forecasts
                2. Classification
                    - a more precise form of clustering (unsupervised learning) where the algorithm is given a set of output baskets to place data items into
                    - mathematically it maps input variables into discrete output variables
            - Quality of training data is essential for supervised ML
                - A dataset can only reveal the relationships it contains
                - But it can induce you to see false or inaccurate relationships
            - The Bombe (built by Turing to crack Enigma codes during the war) is an early example of supervised learning
                - It because successful when the researchers found some _certain_ relationships within the data (different ciphered characters corresponded to the same plaintext)

    - There are two major schools of thought on how far AI can go:
        1. Strong AI - machines can fully replicate human intelligence including consciousness and sentiment
            - this vision is tightly coupled to determinism - the idea that any behaviour is the result of some previous behaviour and the cause of some future behaviour.
                - Q: does this imply that we don't have free will?
            - so in many ways the difference between strong AI and weak AI is philosophical.
        2. Weak AI - machines can only so a simulation of advanced reasoning and learning

## 1.6. The data scientist

-   a major job of a data scientist is to find and transform data so it is good quality for a ML algorithm to train on
-   having domain knowledge is really important when you are trying to get high quality data for a training run
    -   data will be bad if the features are too tightly correlated

Statistics vs ML

-   Statistics uses mathematical methods to find the best fit for **existing** data
    -   Success is graded on how well the model fits the data points - the better the fit, the better the model
-   ML uses the same mathematical methods but grades itself on how well the model can predict unseen data
    -   Success is graded on how well the model performs against unseen data, not how well it fits existing data
        -   Models can be "overfitted" where they descibe the existing data really well but do poorly on unseen data

Authors think the desire for AI is rooted in people wanting to have smarter, more helpful software, software which anticipates their needs

## 1.7. A Taxonomy of ML problems and solutions

We can categorise ML by

1. What is the problem?
2. Does the problem require deep learning (aka some kind of neural network) or shallow learning (everything else)?
    - Some problems e.g. image classification cannot be solved without deep learning methods
    - Neural networks aren't always better but they are always more complicated so only use them if you need them.
    - There is no clear distinction between deep and shallow learning
3. Which algorithm should we use? There may be multiple algorithms which can address the same problem.

Popular ML libraries organise themselves based on these questions

### ML Problems

1. Shallow learning problems
    1. Classification
    2. Predicting Results
    3. Grouping Objects (Clustering)
2. Deep learning problems
    1. Image classification
    2. Object detection
    3. Text Analytics (aka Natural Language Processing)

#### 1. Shallow Learning Problems

1. Classification
    - supervised learning
    - find the category or categories an _object_ belongs to
        - An object is
            - an array of values which represents a data item
            - Value:
                - also called a _feature_
                - a _measurable_ property which makes sense to consider in this scenario
        - A function f where `f(object) -> category` or `f(object) -> category[]`
        - A function which maps input variables to discrete output variables
    - Sub-categories:
        1. Binary Classification
            - there are only two categories available
        2. Multiclass Classification
            - 3-\* categories available
            - objects are put into exactly one category
        3. Multilabel Classification
            - the object can be put into multiple categories at the same time
            - e.g. sentiment analysis on a blog post
        4. Anomaly Detection
            - Find objects whose feature values are _significantly different_ from _the majority_ of other objects in the dataset
            - aka find the outliers
    - Algorithms commonly used for this problem:
        1. Decision Tree
            - a binary tree created by training to process each input object
            - each leaf represents one output category
            - each node has a rule which decides which of it's child nodes the object should be sent to next
            - Training determines:
                1. The number of nodes
                1. The rules at each node
        2. Random Forest
            - A specialised version of decision tree
            - Instead of one tree, the algorithm uses a collection (forest) of simpler trees, all trained differently
            - The response is some sort of average of the outputs of each forest
        3. Support Vector Machine
            - Represent input objects as points in N dimensional space (where objects have N features)
            - Data points are called vectors because they have coordinates within the N dimensional space of the data
            - Look for a sufficiently big distance between points
            - _Support vectors_ aka _extreme data points_
            - In 2D it looks like a curve that cuts the space in two leaving as much space as possible between the two groupings of points
            - https://www.youtube.com/watch?v=Y6RRHw9uN9o
        4. Naive Bayes
            - Computes the probability that a given object (with it's values) would fall into each of the output categories
            - Based on Bayes Theorem
        5. Logistic Regression
            - Computes the probability that a given object (with it's values) would fall into each of the output categories
            - Uses a _sigmoid (logistic)_ function which tends to calculate probabilities very close to either 0 or 1
                - This lends itself well to binary categorization
    - Real world examples
        - Spam detection (binary classification)
        - Customer churn detection (multiclass classification)
        - Creating diagnoses from medical images (binary classification)
        - Recommender systems (multilabel classification)
        - News tagging (multilabel classification)
        - Fraud detection (anomaly detection)
        - Fault detection (anomaly detection)
2. Predicting Results
    - supervised learning
    - All about regression (see jargon section)
    - Sub-categories:
        1. Linear regression
            - algorithm seeks a straight line function to model the relationship between inputs and output
            - a single input guides the prediction e.g. if you have an output which depends on N things then you can only pick just one of those N to be the variable that will guide the prediction
            - ++ simple
            - -- unrealistic for most scenarios
        2. Multilinear regression
            - regression function depends on more than just one input feature/variable
        3. Polynomial regression
            - the relationship between the input values and the output is modelled as an nth degree polynomial in **one** of the input values
                - Q: Does this mean the output still depends on just one input but with a more complicated relationship?
            - a special case of multilinear regression
        4. Nonlinear regression
            - any regression where the output is not modelled by a linear curve
            - basically anything that doesn't fit into the other categories
    - Algorithms commonly used for this problem:
        - the best curve is the one that minimizes the error function (i.e. it minimizes the difference between the predicted output and the actual output)
        1. Gradient descent
            - returns the coefficient which minimize some error function
            - works iteratively by:
                1. Choose default values for the coefficients
                2. Compute the error (using the given error function)
                3. If error is big, look at gradient of the function and move in that direction by choosing new coefficient values
                    - what does this mean?
                4. Go to step 2 until some condition is met
        2. Stochastic dual coordinate ascent
            - solves a different but related problem to minimizing the error
            1. choose an axis
            2. proceed along the axis until it finds coefficient values which will maximise the value of the function
            - Q: what function?
            3. choose the next axis
            4. Go to step 2
        3. Regression decision tree
            - builds a decision tree
            - differs from the classification decision tree in
            - different error function
            - feature value in each node is the mean of all values
        4. Gradient boosting machine
            - combines multiple weaker algorithms (called "weak learners") to build a unified, "strong learner"
                - output is a weighted combination of the output of the chained weak learners
                - the weak learners are often basic decision trees
            - Popular example algorithms in this class
            - LightGBM
            - XGBoost
    - Real world examples
        - Price prediction
        - Production prediction
        - Income prediction
        - Time series forecasting (e.g. in financial, medical, industrial applications)
3. Grouping Objects (Clustering)
    - a form of unsupervised learning
    - the unsupervised cousin of classification
    - similar to classification but it gets **no guidance** from the training set about possible target groups
    - the output is an array of subsets of the dataset
      _ they are not labelled
      _ further analysis is left to the data science team
    - => you will never deploy a clustering algorithm in production
    - its output is used to understand data as part of a larger data pipeline
    - looks for disjoint areas of the dataset (not necessarily partitions) which have something in common
    - most clustering algorithms have linear time complexity except for hierarchical algorithms
    - Uses one of the following approaches
        1. Partition based
        2. Density based
        3. Hierarchy based
    - Commonly used algorithms
        1. K-Means
            - simplest and fastest algorithm
            - is kind of a mix of classification and clustering because it sets a fixed **number** of groups
            - steps:
                1. sets a fixed number of clusters and randomly defines their data centers
                2. go through the dataset and for each data point, calculate the distance from each data center
                3. put that point in the cluster which has the closest center
                4. "proceed iteratively" ??? and re-calculate the data center at each step
        2. Mean-Shift
            - partition based algorithm
            - defines a circular sliding window of arbitrary radius
            - at each step the center point of the circle is shifted to the mean of the points within the radius
            - the method converges when no better center point is found
            - repeat the process until all points fall within a window and overlapping windows are resolved
            - keep only the window with the most points
        3. DBSCAN
            - density based algorithm
            - steps (these are from the book but they don't make sense to me - TODO):
                1. start with first unvisited point in the dataset
                2. find all points within a given range and add them to a cluster
                3. if there aren't enough points, mark the first point as an outlier for this iteration
                4. continue picking points in the cluster and adding all points within a given range of the picked point
                5. iterations continue until there is at least one point not included in any cluster or "their number is so small you can ignore them"
            - ++ more efficient than partition based methods at detecting outliers
            - -- less efficient in clusters of varying density
        4. Agglomerative hierarchical clustering
            - hierarchy based
            - treats each point as a cluster
            - iteratively combines clusters which are close enough to each other (by some distance metric)
            - technically it ends when all points are in a single cluster (the same as the original dataset) but we add some logic to tell it when to stop (e.g. stop after N iteration)
    - Common problems in real world
        - Regrouping news based on topic, author etc.
        - Discovering customer segments for marketing purposes
        - Identifying suspicious fraudulent financial or insurance operations
        - Geographical analysis for city planning or energy power plant planning
        - tagging digital content (video, audio, text)

#### 2. Deep Learning Problems

1. Image classification
    - part of computer vision
    - classifiers have to be trained on the specific data they use
    - there are no general purpose image classifiers yet - they have to be trained on specific data sets e.g. an image classifier from a cloud provider wouldn't do well with medical x-rays
    - typically implemented as a convolutional multi-layer neural network
2. Object detection
    - also part of computer vision
    - used in self-driving cars and robotics
    - commonly two types of learning for the neural network:
        1. classification based learning
        2. regression based learning
3. Text Analytics (aka Natural Language Processing)
    - goal is to let software interact with humans using natural language
    - parsing and tokenizing of text looking for trends
        - identifying sentence boundaries
        - identifying lemmas
        - performing lexical analysis
        - learning relationships between named entities
    - usually built with neural networks

## Automated Machine Learning (AutoML)

-   You tell it your intent and give it your dataset to analyze and it creates a workflow for you using a wizard-style tool

### An example manual ML Workflow

Skills you should already have

1. Domain knowledge
2. Trial and error attitude
3. Imagination

Steps you take

1. Preliminary data analysis
2. Preliminary data cleaning
    - data transformations
3. Identify the features of the data which look most promising and **relevant** to solve the actual problem
4. Select an algorithm
5. Configure the parameters of the algorithm
6. Define an evaluation model to measure the performance of the algorithm
7. indirectly measure the quality of the dataset

AutoML attempts to "wizard" this for you.

Authors take:

-   AutoML does a good job on simple problems
-   Most real-world problems are not simple

    UP TO Common features

# 2. Chap 16

## 2.1. Neural Networks

Uses

-   are "Deep learning"
-   Used when simpler methods don't work

Kinds of neural network

1. Feed forward neural networks
1. Recurrent neural networks
    - ++ stateful
1. Convolutional neural networks
    - e.g. computer vision
1. Generative adversarial neural networks
    - ++ can do content creation

Notes

-   feed forward => information flows only from the input layer to the output
    -   other arrangements are possible
    -   -- feed forward networks are stateless and keep no memory
        -   every prediction is independent of previous predictions
    -   -- cannot handle images or video files
    -   -- cannot generate new content
-   neuron = a computing unit
-   neurons are organised into _homogenous_ layers
-   layers are connected to each other so that info flows from previous layer to next
    -   it is the _connections_ between the layers that mimic synapses in the human brain
    -   each layer of neurons has an _activation function_
        -   the activation function is used to get the output of a neuron
-   the depth of a neural network (in layers) is the "deep" in "deep learning"
-   the input and output of a neural network are vectors of real numbers (arrays of floats)

What is a neuron

-   a function that takes a few values as input and returns a single binary value (usually a float these days)

Types of artificial neuron

1. Peceptron
    - oldest
    - can be used to simulate a NAND gate
    - NAND gates are functionally complete so all other kinds of gates can be implemented as NAND
    - each peceptron is tuned with weights and bias
        - the training process chooses weights and bias which give the (hopefully) desired outcomes

```ruby
# pseudocode

##
# "Weighs" the received input and makes a decision. Returns 1 if the value is above a certain level of confidence
# @param [Array<Float>] xs
# @return [Float] 0 or 1
#
def peceptron(inputs, weights, bias)
    fail if inputs.length != weights.length

    sum_of_products = scalar_multiply(inputs, weights)

    # sum_of_products >= threshold ? 1 : 0
    sum_of_products + bias >= 0 ? 1 : 0
end



nand_weights = [-2, -2]
nand_bias = 3

# this peceptron is a NAND gate if there are only two inputs and they are 0 or 1
peceptron([0, 0], nand_weights, nand_bias)
```
