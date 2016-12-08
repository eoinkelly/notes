# Servo

## Power consumption

* doing work in parallel is better for power consumption!
* it turns out that doing stuff on all cores for less time takes less power
  than running one core for longer
* layout is necessary for the user to see _anything_ on the page so it should
  be prioritzed.


## "Concurrency" vs "fine grained parallellism"

* sometimes there is one task we want to complete as quickly as possible
* concurrency is doing multiple tasks at the same time
* paralellism is doing a single task faster
* because layout is so important we want to parallelise it and put all our CPU
  power towards it

Aside:

    currently iframes run on the main thread - in servo it will be possible for
    them to not be

Aside:

    during layout widths are computed top down, heights are computed bottom up
