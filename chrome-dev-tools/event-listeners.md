# Chrome dev tools

## Event Listeners

All nodes = show events registered on:

1. the currently selected element
2. ancestors of the currently selected element

Selected nodes only = show only handlers registered directly on the element you
have selected

useCapture = true if the event was registered in the "capturing phase" rather
than the default "bubbling phase"

How do I see events that bubble through (but are not registered on) the current
element?

getEventListeners() in console lets you see events on window (which doesn't seem
selectable from UI?)

- can show all event listeners that are

1. attached to a node
2. bubble through the node

TODO: dig more into events in dev tools
https://developer.chrome.com/devtools/docs/dom-and-styles#viewing-element-event-listeners
QUESTION: does the order of events match the order they are executed - test by
manually registering some events

TODO: dig deeper into raw JS events
https://developer.mozilla.org/en/docs/Web/API/Event
