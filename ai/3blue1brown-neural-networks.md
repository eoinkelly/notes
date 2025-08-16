# Neural networks

## Overview

- Sources
    - 1 https://www.youtube.com/watch?v=aircAruvnKk&t=102s
    - 2 https://www.youtube.com/watch?v=IHZwWFHWa-w
- Neurons just contain one number - the activation
- Weights don't live in the neurons, they live on the connections, they
  represent the "thickness" of the connections
- Every neuron has a connection to all the neurons in the layer before it
    ```
    layer 1: 50 neurons
    layer 2: 10 neurons
    => each neuron in layer 2 will have 50 connections to 50 i.e. connected to 50 activations via 50 weights
    ```
- bias
    - each neuron has 1 bias number (there as many bias numbers as neurons
      exluding the input layer)
    - neurons in the input layer do not have biases
    - the bias is a bias for or against activation
    - +10 bias => the weighted sum of activations from previous layer neurons
      and weights must be greater than -10 to get a positive value
    - -10 bias => the weighted sum of activations from previous layer neurons
      and weights must be greater than 10 to get a positive value
    - think of the bias as tendency to be inactive or active
- each neuron is a function
    - which takes in
        1. N activation values from previous layer of N neurons
        2. N weights (conceptually living on the connections between the neurons
           on the previous layer and this neuron)
        3. 1 bias number (assigned to this neuron)
    - outputs
        1. 1 activation value

## Calculations

We use matrix and vector math so we calculate the activations for a whole layer
at a time

Given

$$ w*{n} = \textrm{square matrix of weights for layer n} $$
$$ a*{n-1} =
\textrm{single column vector of activations from layer n-1} $$
$$ b*{n} =
\textrm{single column vector of biases for layer n} $$
$$ a*{n} = \textrm{single
column vector of activations for layer n} $$
$$ relu() = \textrm{squish
function - shown here implies that you apply it to every value in a single
column matrix} $$

Then the activations for the whole of layer $n$ are calculated in a single
operation by:

$$ a*{n} = relu(w*{n} \* a*{n-1} + b*{n}) $$

Moving forward through a network is calculating the activation of each layer in
turn until you get to the activations of the output layer.

The process in pseudocode

```python

def calculate_cost_of_single_training_example(training_example, weights, biases):
    # - Run a single training example forward all the way through the network
    # - The output will be crap because weights and biases were random.
    #     - The output is N activations where there are N neurons in the output layer
    # - Compare the crap output to the expected output for that single training
    #   example using a cost function to quantify how far apart they are
    #     - The expected value must also be expressed as N neuron activations (where there are N neurons in the output layer)
    #     - compute the squared difference between the actual and expected for
    #       each neuron. Then sum that difference across all output neurons to get
    #       a single number
    #         - This number is the "cost" of that single training example
    return cost_of_training_example

def calc_cost_of_all_training_examples(all_training_examples, weights, biases):
    costs = []

    for training_example in all_training_examples:
        costs += calculate_cost_of_single_training_example(training_example)

    avg_cost = average(costs)
    return avg_cost


# Set weights and biases (our "tuning parameters") to random values
weights = random()
biases = random()
training_set = ...

epochs = (1..100)

# each epoch runs all the training data through the network and then tweaks the
# weights and biases
for epoch in epochs:
    # this is the average cost of all training examples for the current values
    # of weights and biases
    avg_cost_of_all_training_exs = calc_cost_of_all_training_examples(training_set, weights, biases)

    # tweak the weights and biases now using back propagation
    weights, biases = back_propagation(avg_cost_of_all_training_exs, weights, biases)

```

The whole cost process can be thought of as a function which takes in all the
weights and biases and spits out a single number describing how they performed
on the training dataset.

You feed the entire training data set through the network every time you tweak
weights and biases.

The training data set is fed through the network as many times as you tweak the
weights and biases - I think each run of train+tweak is called an epoch

### Tweaking weights and biases: back propagation

So we have our weights square matrix (W) and biases column vector (B) and a
single cost number (c).

We want to minimise the cost.

We can think of the cost as being a function

$$ f(W, B) -> c $$

This is a very complex function because there may be thousands or millions of
weights and biases.

How do we find new values for W and B which make c smaller?

If we figure out the slope of the cost function at our current cost c then we
can make a decision about which direction to go.

If you make your step sizes proportional to the slope then you can take smaller
steps are the slope gets flatter i.e. you approach a local minimum.

The gradient of a function gives you the direction of steepest increase. The
reverse of gradient is what we want.

1. Compute the gradient direction of the cost function
2. Nudge the inputs in that direction to make the cost smaller

Imagine all our weights and biases are a single column vector. The negative
gradient of that vector will also be a vector of the same shape - this is the
vector of "nudges". We can apply the nudges to the weights and biases

The algorithm for computing this gradient efficiently is called "back
propagation"

We need the cost function to have a smooth output so that we can make steps.
This is why activations are floats not boolean.

### Back propagation

#### Back propagation works on a single example

Steps

1. Starting with the output layer, calculate the vector of nudges you want to
   happen to the layer before it
    - go through each output layer neuron and calculate which nudges it wants to
      all the neurons in the previous layer
        - do this by changing the N-1 layer weight for each neuron proportional
          to the activation of that neuron (more active neuron => changing
          weight has more impact)
        - add or subtract from the bias based on what you want to happen to the
          output of this neuron
    - then sum up all the desired nudges into a single 1D vector of nudges for
      the previous layer
2. Repeat this process for each layer, working backwards from output to input

```python

def do_back_prop_for_one_training_ex(training_ex):
    nudges = []

    for layer in reverse(layers):
        nudges_desired_by_neurons_in_this_layer = []
        for neuron in layer:
            nudges_desired_by_this_neuron = []
            # calculate the nudges this neuron wants to make to the previous layer
            # by
            # 1. changing the n-1 layer weight for each neuron proportional to the
            #    activation of that neuron (more active neuron => changing weight has
            #    more impact)
            # 2. Add or subtract from the bias based on what you want to happen to the output of this neuron
            #
            # You need to have a set of desired values for the output neurons to start this (this is the single training example)
            nudges_desired_by_neurons_in_this_layer << nudges_desired_by_this_neuron

        # sum all the nudges desired by each neuron into one desired nudge to each weight and bias in the previous layer
        nudges_desired_by_this_layer = sum(nudges_desired_by_neurons_in_this_layer)
        nudges << nudges_desired_by_this_layer

    # After this nudges has a nudge for each eight and bias in our system
    # These nudges represent the changes that would improve the weights and biases for this single example we used
    return nudges
```

### Doing back prop for all training examples (real gradient descent)

This process changes many/most of the weights as part of a single back prop run.

Strictly speaking Back prop has to be repeated for **every training example**.

You get a new set of nudges for weights and biases from every example.

Then we **average** all those new nudges together to get one final set of
nudges.

```python
nudges_for_all_training_exs = []
for training_ex in training_exs:
    nudges_for_all_training_exs << do_back_prop_for_one_training_ex(training_ex)

final_nudges = avg(nudges_for_all_training_exs)

# apply the nudges for the next epoch of training
weights, biases = apply_nudges(weights, biases, nudges)
```

### Doing back prop on batches of the training data set (stochastic gradient descent)

In reality for computational efficiency the training set is divided up into
chunks called _batches_ and back prop happens on the chunk not the whole
training set.

The next gradient descent step (which is the vector of nudges averaged from all
training examples in the batch) is calculated from the training examples in the
batch.

    Q: Do we do just one batch or do we do each batch and somehow combine the numbers

This is not exactly true gradient descent because we don't do it over all
training examples. This batched version is called **Stochastic gradient
descent**

This causes the steps we take to be faster but less exact than true gradient
descent but it gets there in the end and is much faster computationally.
