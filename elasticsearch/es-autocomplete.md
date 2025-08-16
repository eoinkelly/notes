# N-Gram and Autocomplete

- [N-Gram and Autocomplete](#n-gram-and-autocomplete)
    - [N-Gram Background](#n-gram-background)
        - [Aside: Using n-grams to model natural language](#aside-using-n-grams-to-model-natural-language)
    - [Elasticsearch](#elasticsearch)
        - [Completion suggesters](#completion-suggesters)
        - [Exploring edge_ngram analyzer](#exploring-edge_ngram-analyzer)

## N-Gram Background

- A concept from Natural language processing (NLP)
- Is just the generalised idea of a moving window of "items" in a stream of
  text, where items can be various parts of text.
    - the items are often words in NLP (but not alwasys)
- a contiguous sequence of _n_ items from a given sample of text or speech
- Items can be:
    - phoneme
    - syllable
    - letter
    - word
    - base pair (in RNA etc.)
- n-gram is an idea from the fields of:
    - Computational linguistics
    - Probability
- Different sizes of n-gram have common names: | n | name | | --- |
  ---------------- | | 1 | unigram | | 2 | bigram or digram | | 3 | trigram | |
  4 | four-gram | | 5 | five-gram |
- Aside: In computational biology a "polymer" or oligomer" of a known size is
  called a _k-mer_ instead of an ngram | k | name | | --- | -------- | | 1 |
  monomer | | 2 | dimer | | 3 | trimer | | 4 | tetramer | | 5 | pentamer |
- If the items are words, n-grams can also be called shingles
- In Elasticsearch the phrase "n-gram" usually refers to characters and the word
  "shingle" is used to refer to words

### Aside: Using n-grams to model natural language

https://www.youtube.com/watch?v=Saq1QagC8KY

- The goal of a language model is to _assign a probability to a sentence_. This
  is useful to
    - Decide between two options in a machine translation (keep the more
      probable one)
    - Spelling correction
    - Grammar correction
    - Speech recognition
- We want to be able to calculate the probability of what the next word in a
  sequence will be
- To do it properly, the probability of the next word depends on all the
  previous words
- But that is very hard to do
- So Markov made an assumption that we could get a "good enough" probability of
  the next word by just looking at the more recent N words in the sentence
- This is the n-gram language model
- It's not great
- Variations of the model
    - Unigram model (n = 1)
        - We guess each word without looking at any of the previous words
        - predictably it's shit
    - Bigram model (n = 2)
        - We look at the previous word to guess the next
        - Not as bad as unigram but still not good
    - Trigram
        - We look at the previous two words to guess the next
        - Better than bigram but still not great
- n-gram models are not very good models of language because language has very
  distant dependencies

## Elasticsearch

- n-grams are used to implement index-time search-as-you-type
- treat the input as an n-gram and use it to find results
- you need to create a custom analyzer to implement this n-gram stuff

```bash
# original word
"star"

# has the following n-grams
unigram: [s, t, a, r]
bigram: [st, ta, ar]
trigram: [sta, tar]
4-gram: [star]

# has the following edge n-grams
edge unigram: [s]
edge bigram: [st]
edge trigram: [sta]
edge 4-gram: [star]

when user types 's'
  we match the 's' edge unigram
user types 'st'
  we match the 'st' edge bigram
user types 'sta'
  we match the 'sta' edge bigram
etc.
```

Edge n-grams are built only on the beginning of each term

```jsonc
PUT /movies
{
  settings: {
    analysis: {
      filter: {
        // create a token filter which is a customisation of built-in edge_ngram
        autocomplete_filter: {
          type: "edge_ngram",
          min_gram: 1,
          max_gram: 20
        }
      },
      analyzer: {
        autocomplete: {
          type: "custom", // create a custom analyzer
          tokenizer: "standard", // with the standard tokenizer
          // and a chain of filters, first make everything lowercase and then through the edge_ngram filter
          filter: [
            "lowercase",
            "autocomplete_filter"
          ]
        }
      }
    }
  }
}

// next ue the analyzer in our mapping
PUT/movies/_mapping
{
  properties: {
    title: {
      type: "text",
      // this will cause the title string to be sent through the autocomplete
      // analyzer which will create edge-ngrams for every 'term' it finds
      analyzer": "autocomplete"
    }
  }
}

// NB: now we need to make sure that we DO NOT use the autocomplete analyzer on
// the query string - this is an example of where the field analyzer is
// different to the query analyzer
GET /movies/_search
{
  "query": {
    "match": {
      "title": {
        "query": "sta",
        // without this the query string would be split into n-grams too giving the wrong results
        "analyzer": "standard" // <-- NB very important
      }
    }
  }
}
```

### Completion suggesters

You can upload lists of autocompletes to make the completions even better.

### Exploring edge_ngram analyzer

The `edge_ngram` analyzer creates edge n-grams **for each token**. The string is
first tokenized and then the edge n-grams are computed! The output tokens are
the edge n-grams.

Example:

```jsonc
GET /movies/_analyze
{
  "tokenizer": "standard",
  "filter": [
    {
      "type": "edge_ngram",
      "min_gram": 1,
      "max_gram": 4
    }
  ],
  "text": "Star Wars"
}

// returns
{
  "tokens": [
    {
      "token": "S",
      "start_offset": 0,
      "end_offset": 4,
      "type": "<ALPHANUM>",
      "position": 0
    },
    {
      "token": "St",
      "start_offset": 0,
      "end_offset": 4,
      "type": "<ALPHANUM>",
      "position": 0
    },
    {
      "token": "Sta",
      "start_offset": 0,
      "end_offset": 4,
      "type": "<ALPHANUM>",
      "position": 0
    },
    {
      "token": "Star",
      "start_offset": 0,
      "end_offset": 4,
      "type": "<ALPHANUM>",
      "position": 0
    },
    {
      "token": "W",
      "start_offset": 5,
      "end_offset": 9,
      "type": "<ALPHANUM>",
      "position": 1
    },
    {
      "token": "Wa",
      "start_offset": 5,
      "end_offset": 9,
      "type": "<ALPHANUM>",
      "position": 1
    },
    {
      "token": "War",
      "start_offset": 5,
      "end_offset": 9,
      "type": "<ALPHANUM>",
      "position": 1
    },
    {
      "token": "Wars",
      "start_offset": 5,
      "end_offset": 9,
      "type": "<ALPHANUM>",
      "position": 1
    }
  ]
}
```
