# react js

react does not have a hard dependency on jquery but it is sometimes used
together as jquery provides wrapper over

- ajax calls

Each component has

- immutable props that it gets from its parent
- mutable `state` that it manages for itself

Lifecycle _ state callbacks _ getInitialState _ render callbacks _
componentWillMount (before render) _ componentDidMount (after render) _
componentWillUnmount \* componentDidUnmount

How does a react component signal something to another component farther up the
chain? no mutable state

## Mapping how components communicate

Parent -> Child _ just pass props Grandparent -> Child _ pass props though
Parent Child -> Parent _ Parent binds one of its functions to itself using
.bind() and passes that bound function to the child _ or use "global event bus"
Child -> Grandparent _ Use global event bus _ Could potentially pass bound
functions along but would be cumbersome Child -> Any \* use a "global event
bus" - subscribe to events in componentDidMount() and call setState() based on
those events. Unsubscribe to the events in compoentWillUnmount()

Flux is a "global event bus" pattern

# FLux

Action -> Dispatcher -> Store -> View

Action _ comes into the system _ could come from view, network etc. Dispatcher _
traffic controller Store _ data layer _ manages all data View _ re-renders
whenever the store says something has changed \* is a source of actions

Flux is different to how Backbone does it in that Flux enforces more
synchronicity. It does seem like it would be possible to use a flux style with
Backbone but at that point you are probably better using something like Fluxxor.

# Implementations of Flux

Fluxxor provides an implementation of flux

- http://fluxxor.com/
- https://github.com/spoike/refluxjs (similar to fluxxor)

# react-router

- https://github.com/rackt/react-router
- react-router provides a similar router to Ember
