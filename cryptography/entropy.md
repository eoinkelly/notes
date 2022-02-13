# Entropy

* Entropy is often discussed in the context of predicting the next symbol in a stream of symbols.
* The generation of each symbol is an _event_.
* Each event has $n$ possible _outcomes_.
* Examples of a **single** event:
    * a coin toss (possible outcomes = 2)
    * a bitstream generator (possible outcomes = 2)
    * a byte stream generator (possible outcomes = 256)
    * the generation of a 128 bit random value (possible outcomes = $2^{128}$)
        * NB: The generation of the random 256 value is considered a single event. It's easy to get this confused when looking at descriptions of entropy from different sources.

Consider an event (singular) which has $N$ possible outcomes

$$N$$

Each outcome has a probability that it will happen between 0 and 1 (usually expressed as a fraction)

$$p_1, p_2, ..., p_N$$

By definition, the sum of the probabilities for all outcomes must equals 1

$$p_1 + p_2 + ... + p_N =  1$$

A **uniform distribution** is when all the probabilities are equal. Each probability is $\frac{1}{N}$. Each outcome is equally likely.

## How much surprise is in a single outcome?

Think about "surprise" and how we might measure the surprise of a single possible outcome using numbers

* Surprise goes up as the probability of the outcome goes down
* Surprise goes down as the probability of the outcome goes up
* => so intuitively surprise and probability have a roughly inverse relationship

So we might try to model surprise as:

$$Surprise = \frac{1}{Probability}$$

but this has edge cases e.g. when probability is 1 we expect surprise to be 0 but

$$Surprise_{p=1} = \frac{1}{1} = 1 $$

which is a bummer. But it turns out the $log_2$ function can help make a better model:

$$Surprise = log_2(\frac{1}{Probability})$$

    # wolfram alpha
    plot log_2(1/x), 1/x from x = 0 to 1.1


Remember we are measuring the surprise of a single possible outcome here. The choice of base in the log doesn't relate to the number of possible outcomes e.g. you don't choose base 4 if you have 4 possible outcomes or base 256 if you have 256 possible outcomes.

I _think_ Shannon picked $log$ because he needed a function which gave an output of 0 when the input is 1 and because logs match our intuition about systems. Many things in a system vary linearly with the logarithm of the number of possible outcomes e.g. adding one relay to a system doubles the number of possible states.

This characteristic is true of the log function no matter what base you choose so in some ways the choice of base does not matter.

Conventionally we use base 2 because we deal with binary computer systems and base 2 makes the entropy measurement more intuitive but other bases are possible:

> The choice of base for the logarithm, varies for different applications. Base
> 2 gives the unit of bits (or "shannons"), while base e gives "natural units"
> nat, and base 10 gives units of "dits", "bans", or "hartleys"
> Wikipedia

## How much surprise is in an event?

So we have a model for measuring the surprise of a single possible outcome of an event. How do we extend that to measure the total surprise of the event?

We might try summing the surprise values:

$$s_{total} = s_1 + s_2 + ... + s_N$$

    Q: how do you pick which side of = to put things? is there a convention?

but this is a bad choice because ???

If we sum the surprise values weighted by the probability of that event occurring then we get a better metric for how much surprise is in the event. This weighted sum of surprise is called _Entropy_ and given the symbol $\Eta$ (_Eta_, the greek capital letter H)

$$\Eta = (p_1 \times s_1) + (p_2 \times s_2) + ... + (p_N \times s_N)$$

Plugging back in our formula for surprise:

$$\Eta = (p_1 \times log_2(\frac{1}{p_1})) + (p_2 \times log_2(\frac{1}{p_2})) + ... + (p_N \times log_2(\frac{1}{p_N}))$$

## Equation refactoring

Tidying up the notation:

$$\Eta = \sum_{i=1}^{N} p_i \times log_2(\frac{1}{p_i})$$

Using the rules of logs to refactor the equation into it's most common (but less clear to me) notation:

$$\Eta = - \sum_{i=1}^{N} p_i \times log_2(p_i)$$


TODO: Actually read the paper fully :-)

## Sources

* https://people.math.harvard.edu/~ctm/home/text/others/shannon/entropy/entropy.pdf
* https://en.wikipedia.org/wiki/Entropy_(information_theory)
* https://www.youtube.com/watch?v=YtebGVx-Fxw