# Bloom filters

* a data structure
* usually implemented as a vector or array of bits
* used to test whether an element is a member of a set
* space efficient - use when it would not be practical to use a standard hash or set data structure
* can have false positives, cannot have false negatives
* when you query the structure for element X you get back one of
    * X is definitely not in the set
    * X is possibly in the set (can't be sure we don't have a false positive)
* elements can be added to the set but not removed
    * if you use a _counting filter_ then you can also remove elements from the structure
* concieved by Howard Bloom in 1970
* variations that store TTL values with each bit exist and are useful for very
  fast/large datastreams where a normal bloom filter would fill up and give all
  false positives too soon
* bloom filters have a maximum on the number of things you can add - a "strict"
  variant prevents you from adding too many
* usage ideas:
    * use a bloom filter as the first step in a search or a huge set of logs
      .e.g we have N directories of daily log files and we are looking for a
      particular name so maintain N bloom filters (one per day) that can be
      searched first and will weed out days that don't include the search term.

> fewer than 10 bits per element are required for a 1% false positive
> probability, independent of the size or number of elements in the set


## Implementations

* Ruby
    * https://github.com/igrigorik/bloomfilter-rb
        * the redis backend adds an order of magnitude to the storage requirements
        * a similar approach could be used without redis (just using a timestamp
        * includes a counting filter implementation
    * https://github.com/colinsurprenant/bloombroom
    * https://github.com/deepfryed/bloom-filter
        * less full-featured than bloomfilter-rb
* Javascript
    * Node can implement them using Buffers or using redis as back-end
    * There are a few things on npm
