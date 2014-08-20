Console API
===========

IDEA: would be good to have the default page in my browser load up some handy sortcuts for the console. the page could display info about how to use them.
e.g.
  load up jquery
  display info about $.ajax usage
https://developers.google.com/chrome-developer-tools/docs/console

console.log(myObject) // shows XML representation of it by default
console.dir(myObject) // shows JS representation of the objecct
dir(myObject) // shortcut for console.dir
console.log("%O", myObject) // shows XML representation of it (same as console.dir())

console.dirxml(myObject) // prints XML representation of the object. same as console.log for HTML elements
dirxml(myObject) // shortcut

console.count // same as console.log but append the no. of times this instance of the call has been called

// Start and stop a timer in the console
console.time()
console.timeEnd()

console.log() has formatting controls like C printf()

console.group() // start a group
console.groupCollapsed() // start a group
console.groupEnd(); // end a group

console.error()
console.warn()
console.debug() // identical to console.log
console.info()
console.log()

console.log("%cHello", "color: #bada55; font-size: x-large;");

console.timeStamp('Some thing happened') // creates an event in the Timelien tab and annotates it with 'Some thing happened'

$('#css-selector') // part of console API NOT jQuery!!!, is an alias for document.querySelector()
  be careful as if jQuery is on the page then it will take over $
  you can test for jquery with
    $('window') instanceof jQuery

$$('#css-selector') // returns array of all elements that match - an alias for document.querySelectorAll()

copy(object); copies a string representation of the object to the clipboard

$x('//xpath/expression') // matches DOM elemnets based on the xpath expression
$_ // the result of the last evaluated expression

inspect(); // show the argument in the appropriate panel

$0 --> $5 // the 5 most recently selected DOM elements

// Start watching for DOM events on specified nodes
monitorEvents(objectToWatch, "event")
monitorEvents(objectToWatch, ["event", "other-event"])
unmonitorEvents(object)

getEventListeners(window) // gets an array of the event listeneres registered for the given DOM object

keys(object)
values(object)


profile("A") // start a CPU profile called A
profileEnd("A") // end a CPU profile called A

