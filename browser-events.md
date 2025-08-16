# Browser Events

There are 3 distinct phases to how events are handled

1. Capture phase
2. Target phase
3. Bubble phase

More; http://www.w3.org/TR/DOM-Level-3-Events/#dom-event-architecture

window.addEventListener(... someDomNode.addEventListener( ...

// Set the onclick attribute of the DOM Node object button.onclick(function (e)
{ });

- Downside: can only register one callback for each event
- button.addEventListener('click', function (e) { ... })

- addEventListener allows you to regiter multiple callbacks for a single event
- on a single node
-

var thing = function (e) { ... }; button.AddEventListener('click', thing);
button.removeEventListener('click', thing);

- You have to name the handler function to be able to remove it

event.preventDefault()

- For _most_ events default action is performed _after_ the handlers are run
    - On chrome you cannot intercept keyboard shortcuts to close current tab

event.stopPropagation()

- stop the event bubbling up beyond the current node
- browser will still run all handlers attached to current node ???

event.target

- the dom node that had _focus_ when the event was fired
- normal nodes cannot have focus unless you give them a _tabindex_ attribute
    - Implication: give a node tabindex to make it an event source
    - ?? is stuff bubbled through it too?
- these types of nodes can have focus by default
    - links
    - buttons
    - form field
    - document.body

- keypress is for getting what the user typed

// To get a key out of an input keypress input.onkeypress(function (e) {
console.log(String.fromCharCode(e.charCode)); });

- Whenever an element is scrolled the 'scroll' event is fired on it
    - It doesn't happen if element is `position: fixed`

elements receive 'focus' and 'blur' when they get/lose focus these events do not
propagate to parents! the window object gets focus/blur when user tabs to/from
it

load _ fired on both window and document body when the page loads _ external
scripts adn images also have load events

beforeunload

requestAnimationFrame _ schedules code to run just before the next redrawing _
sort of a specail setTimeout \* cancelAnimationFrame

setTimeout clearTimeout

setInterval clearInterval

TODO: dig into browser events properly
https://developer.mozilla.org/en-US/docs/Web/Events

# Network Events

Come from the browsers Network interfaces. These are:

1. XmlHttpRequest
2. Web sockets
3. Web workers
4. Server sent events
5. others ???

Examples of events:

```
web sockets/web worker/server-sent-event:
  message

xhr:
  loadstaart
  loadend
  timeout
```
