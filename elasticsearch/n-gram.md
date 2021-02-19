# n-gram

* A concept from Natural language processing (NLP)
* Is just the generalised idea of a moving window of "items" in a stream of text, where items can be various parts of text.
    * the items are often words in NLP
    * In Elasticsearch the items seem to be characters
* a contiguous sequence of _n_ items from a given sample of text or speech

Items can be:

* phoneme
* syllable
* letter
* word
* base pair (in RNA etc.)

If the items are words, n-grams can also be called shingles

n-gram is an idea from the fields of:

* Computational linguistics
* Probability

Different sizes of n-gram have common names:

| n   | name             |
| --- | ---------------- |
| 1   | unigram          |
| 2   | bigram or digram |
| 3   | trigram          |
| 4   | four-gram        |
| 5   | five-gram        |

In computational biology a "polymer" or oligomer" of a known size is called a
_k-mer_ instead of an ngram

| k   | name     |
| --- | -------- |
| 1   | monomer  |
| 2   | dimer    |
| 3   | trimer   |
| 4   | tetramer |
| 5   | pentamer |

## Language models

https://www.youtube.com/watch?v=Saq1QagC8KY

The goal of a language model is to _assign a probability to a sentence_.
This is useful to

* Decide between two options in a machine translation (keep the more probable one)
* Spelling correction
* Grammar correction
* Speech recognition

### n-gram models

We want to be able to calculate the probability of what the next word in a sequence will be
To do it properly, the probability of the next word depends on all the previous words
But that is very hard to do
So Markov made an assumption that we could get a "good enough" probability of the next word by just looking at the more recent N words in the sentence
This is the n-gram language model
It's not great

* Unigram model (n = 1)
    * We guess each word without looking at any of the previous words
    * predictably it's shit
* Bigram model (n = 2)
    * We look at the previous word to guess the next
    * Not as bad as unigram but still not great
* Trigram
  * We look at the previous two words to guess the next
  * Better than bigram but still not great

n-gram models are not very good models of language because language has very distant dependencies