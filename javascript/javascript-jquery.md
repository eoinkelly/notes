# Inbox

http://docs.jquery.com/Plugins/Authoring
http://msdn.microsoft.com/en-us/magazine/gg723713.aspx (deffered and promises)
http://jqueryenlightenment.com/

dom scriptitng, not jquery: http://www.domenlightenment.com/

# The .each() "pattern"

setting up event handlers happens at DOMReady. It may be useful to use
$('foo').each() to iterate over all the elements you want add events to because
you can "pre select" the DOM elements and they will be available in the handlers
via closure

- This is based on the idea that any code that touches the DOM e.g. jQuery
  selectors are expensive.

// Option 1: Does more work at DocumentReady to do less work on each click // \*
better if you know the buttons will be clicked a lot $("button").each(function
() { var $button = $(this); // get a jQuery wrapped version of the element we
care about var $num = button.find('.number'); // find some sub-element
button.on('click', function (event) { // $button, $num are available in all
these handlers (through closure) and only had to be computed once on the page
}); });

// Option 2: Does less work at DocumentReady but more work on each click // \*
better if you think buttons might not be clicked much $("button").on('click',
function (event) { // note that in this version we have to go through sizzle to
find the elemnets for each and every button on the page. // If there are a lot
of buttons, this could be a lot more work. var $button = $(this); // get a
jQuery wrapped version of the element we care about var $num =
button.find('.number'); // find some sub-element });

// Option 3: Lazy evaluation - We only do the sizzle search on the first click
// _ probably best compromise solution if you are not confident that all the
buttons will be clicked $("button").each(function () { // _ These guys are
available to the handler via closure. // \* Each button handler will have a
unique copy of these variables var
$button,
        $num;
    button.on('click', function (event) {
        if (! $button) {$button
= $(this); }
        if (! $num) {$num = $button.find('.number'); } // Do stuff
... }); });

// Option 4: Event Delegation // _ Rather than binding an event to each button,
we bind a single event to the parent form // _ This means less events for the
browser to manage so is more efficient // \* A big win is that this will work
for any future buttons added to this form $('form').on('click', 'button',
function (event) { event.preventDefault(); var $button = $(this); var $num =
button.find('.number'); // Do stuff ... });

// Option 5: Event Delegation with selectors stored in jQuery .data()
$('form').on('click', 'button', function (event) {
    event.preventDefault();
    var $button = $(this);
    var $num = button.find('.number');
    var data = $button.data('countdown');
    if (! data) {
        data = {}
        data.num = $num
        data.number = Number($num.text()) -
1; button.data('countdown', data); } // Do stuff ... if (data.number === 0) {
$button.prop('disabled', true); $button.removeData('countdown'); } });

# .prop() vs. .attr()

The .prop() method should be used for boolean attributes/properties and for
properties which do not exist in html (such as window.location). All other
attributes (ones you can see in the html) can and should continue to be
manipulated with the .attr() method.

'className' is the property 'class' is the attribute

- some properties are defined in HTML attributes e.g. class

http://blog.jquery.com/2011/05/12/jquery-1-6-1-released/
http://stackoverflow.com/questions/5874652/prop-vs-attr
http://ejohn.org/blog/jquery-16-and-attr/

.prop() \* returnst he property for only the **first** element in the set.

selectedIndex tagName nodeName nodeType ownerDocument defaultChecked
defaultSelected

    * In IE6-8 do not set the value of a property to anything other than a primitive type or you will cause memory leaks

# .data()

- used to get/set arbitray data on a DOM element
- data attributes will be made available via this funciton too

jQuery Philosophy

1. Find something
2. Do something

# .proxy()

- takes a function and an object and binds the object to 'this' every time that
  function is called in the future

when an event handler is invoked jquery makes ure the this pseudo paramter is
set to the DOM element that called the event

returning false from an event handler cancels bubbling and prevents the default
action. When you return false you are possibly breaking any code that is waiting
for that event on nodes further up the DOM

# .bind()

- ES5 method - not all browsers ahve it .click(func) i same as .bind('click',
  func)

.on replaces .bind, .live, .delegate

http://coding.smashingmagazine.com/2012/05/31/50-jquery-function-demos-for-aspiring-web-developers/?utm_source=javascriptweekly&utm_medium=email

make jquery available in chrome console for a site that does not include it var
jq = document.createElement('script'); jq.src =
"http://code.jquery.com/jquery-latest.min.js";
document.getElementsByTagName('head')[0].appendChild(jq); jQuery.noConflict();

# The jQuery Object

a jquery object seems to be a collection of DOM elements stored in numerically
name properties (0,1, 2 etc.) as well as a fancy prototype and a few other bits
of "array like" stuff

# $.data

jquery lets you associate arbitrary data with a DOM node

$(“#mydiv”).hide();
$(“#mydiv”).css(“padding-left”, “50px”);

this is faster because jquery doesn't have to go through sizzle multiple times
$(“#mydiv”).hide().css(“padding-left”, “50px”);

// Cache a jquery selector so it doesn't have to search more times than needed
var
$mydiv = $(“#mydiv”).hide();
[.. lot of cool stuff going on here …]
$mydiv.show();

prime directive of an event loop: never block, never wait, finish fast

# jQuery.fn

jQuery.fn is an alias to jQuery.prototype

- any properties and methods on jQuery.fn is available to any jquery object
  jQuery.fn.eoinsPlugin

        more at http://stackoverflow.com/questions/4083351/what-does-jquery-fn-mean

fn object, which is a special jQuery construct made for authoring plugins.

so if your plugin will target specific elemnts on the page, you should use
jQuery but if it will act on any given jquery object, use jQuery.fn ?

'jQuery' is the object itself, a function. 'jQuery.fn' is its prototype. every
jquery object inherits the prototype methods, so u'll need to extend the .fn,
not the object itself (it would be like adding static stuff to a class).

Also, if it isn't clear from the other responses, you can add plugin methods
without using the extend method: jQuery.fn.myPlugin = function(opts) { ... }

It’s important to know that if you want to create the plugin for specific kind
of inputs you must extend from jQuery object to manage and specify elements in
your future try.js file. Remember that jQuery object doesn’t manage specific
elements.

There are 2 basic objects that we will extend:

jQuery: mainly handles the internal processing. jQuery.fn: handles the
interaction of elements.

So if We want to create a general function like $.post we must use jQuery
object. But if We need to create a new kind of animation for our elements, using
the power of the jQuery selectors (for DOM’s elements) we would extend the
jQuery.fn object.

// It seems that by attaching the new variable to the $.fn object (rather than
$) // we get the 'this' set to the jquery object that the selector got us

# jQuery Plugins

https://github.com/cowboy/talks/blob/master/jquery-plugin-authoring.js
http://benalman.com/news/2010/11/immediately-invoked-function-expression/

$.fn.newMethod = function () {

// This allows our newMethod to be chained return this; }

http://www.learningjquery.com/2007/10/a-plugin-development-pattern a way of
handling options ===================== // plugin definition $.fn.hilight =
function(options) {   // Extend our default options with those provided.   //
Note that the first arg to extend is an empty object -   // this is to keep from
overriding our "defaults" object.   var opts = $.extend({},
$.fn.hilight.defaults, options);   // Our plugin implementation code goes here.
}; // plugin defaults - added as a property on our plugin function
$.fn.hilight.defaults = {   foreground: 'red',   background: 'yellow' };

    // this need only be called once and does not
    // have to be called from within a 'ready' block
    $.fn.hilight.defaults.foreground = 'blue';

http://docs.jquery.com/Plugins/Authoring

# The jQUery Object

it contains a collection of DOM elements (chosen by selector or created) it is
"array like" has a .length property elements in the object can be accessed by
numeric indices it is NOT a real array so array functions like .join will not
work it can be empty - create using $() or if a selector matches no elements An
array-like object is an object using numeric keys and having a length property -
that is the minimum needed for compatibility with the native Array methods.

To get a raw DOM element from jquery use $('#element').get(0);

On inspection a jquery wrapped element has 0 = the raw DOM element context =
another pointer to the DOM element???

## The DOM

https://developer.mozilla.org/en/Gecko_DOM_Reference

An element in the Document Object Model (DOM) has attributes, text and children
It provides methods to traverse the parent and children and to get access to its
attributes

There are a number of defined _Interfaces_ in the dom e.g.

- Node
- Element (inherits from Node)
- HTMLTableElement
- Window
- Document

Objects in the DOM usually implement more than one of these e.g. a table would
implement all 3 of the above

The interface you use will be the root of the funciton call

Document interface document.getElementById document.createElement Element
interface https://developer.mozilla.org/en/DOM/element
https://developer.mozilla.org/en/DOM/Node element.setAttribute
element.getAttribute element.getElementsByTagName Window interface
window.content window.onload HTMLTableInterface interface table.border
table.summary table.createCaption table.InsertRow

All elements implement Node, Element but also may have specific interfaces for
their content e.g. Table

DOM Data types The following data types are passed around the DOM

- document
- element
- nodeList
- attribute
- namedNodeMap

Doug Neiner has a good talk on executing code as late as possible i.e. don't run
all your setup in $(document).ready() especially stuff the user might never
interactwith

- Good example: not loading social media button code until the user mouses over
  the area where they are

# One Time Initialization Patterns

Basic Variable Flag jQuery one() Class Name Based Class Name + Delegation

Advanced Deferred Finite State Machine

## Variable Flag

var $dialog; // $ prefix indicates it will be a jQuery object, gets initialized
to undefined (which is falsy)

$("#contactUs").on("click", function () { if (! dialog) { $dialog =
$("contact").dialog(); // dialog() auto opens by default so we don't have to
explicitly call open } else { $dialog.dialog('open'); } });

## jQuery one()

var $dialog;
$('#contact-us').one('click', function () { $dialog = $('contact')
.dialog({ autoOpen: false }); .on('click', function () { $dialog.dialog('open');
}); });

## Class Name Based

/\*

- This stores the state of whether we have run our setup or not in the DOM (vs.
  in a variable which the Variable Flag method does) \*/
  $(document).on('mouseenter', '.slideshow', function () { var $el = $(this); //
  jQuery sets this = to the DOM element the event was triggered on if (!
  $el.hasClass('is-ready')) { // do stuff $el.addClass('is-ready'); // add the
  class which will function as a flag for us } } );

## Class Name + Delegation

/\*

- Used the selector to target the element iff it doesn't have a particular class
  \*/ $(document).on('click', '.slideshow:not(.is-ready)', function () { // do
  stuff $(this).addClass('is-ready'); // add the class that indicates our state
  } );

## Deferred

?? to research

## Finite State Machine

?? to research

# Event Delegation in jQuery

.bind used to implement the other methods i think

.live

$(selector).delegate(filter, eventName, callbackFunction) /\*

- The event handler is actually bound to whatever the selector gives us
- the filter acts as a filter to only handle events that come from dom elements
  that match it \*/

$(elements).delegate(selector, events, data, handler);  // jQuery 1.4.3+
$(elements).on(events,
selector, data, handler); // jQuery 1.7+

.undelegate

.on() replaces .delegate(), .live()

.off() // opposite of .on()o

For best performance, attach delegated events at a document location as close as
possible to the target elements. Avoid excessive use of document or
document.body for delegated events on large documents.

.one() a variation of .on() that binds an aevent that only ones once and then
detaches itself

# jQuery Deferred

Based on CommonJS promises/A proposal

A state storage object. It has 3 states:

1. pending (initial state on creation)
2. resolved
3. rejected

var dfd = $.Deferred(); // Create deferred object with initial state is
'pending'

dfd.state(); // displays state FIXME doesn't work???

dfd.resolve() // sets state to 'resolved' dfd.reject(); // sets state to
'rejected'

// Attach a callback to when the state changes to 'done' dfd.done(callbackFunc);
// Will call callback when the state changes to 'resolved'

// Attach a callback to when the state changes to 'rejected'
dfd.fail(callbackFunc); // Will call callbackFunc when the state changes to
'rejected'

dfd.always(callbackFunc); // Attach a callback to be run whenever the state is
no longer 'pending'

dfd.promise() // return the promise object of that deferred The deferred is the
whole state objects A promise is a subset of the deferred object - it exposes
the ability to attach callbacks to it but not change it's state

- A promise is an immutable view of a deferred object which can be passed around
  safely as the underlying deferred cannot be resolved or rejected through it.

$.ajax() // returns a "deferred like" object that we can use like a deferred
$.getJSON()
// returns a "deferred like" object that we can use like a deferred

- Any object that isn't a deferred can be treated like one and will become an
  immediately resolved deferred
- Callbacks attached to deferreds always get the same arguments
- Generally you want to return promises unless you know you need the full
  deferred.
- When in doubt return a deferred, better yet return a promise
- deferreds are very powerful as properties stored on objects

## Making a pretty alternative to setTimeout()

$.wait = function (duration) { return $.Deferred(function(def) {
setTimeout(def.resolve, duration); }); }

function doStuff () { console.log('doing stuff'); }

$.wait("800").then(doStuff);

## Trigger state changes

dfd.reject(args) dfd.rejectWith(ctx, args) resolve(args) resolve(ctx, args)
.notify() // fires a 'progress' event so all handlers attached to the 'progress'
event on the deferred will be called.

## Utilitiy Helpers

.pipe() .then() .when() // takes multiple deferred objects and returns a single
deferred object that represents all of them

$.when( getAddress(); getTweets(); ).then( renderStuff(); )

var watcher = $.Deferred();

// Attach a progress handler watcher.progress(function (progress, limit) { if
(progress === (limit - 50)) { console.log('Halfway there!'); } });

// Attach a done handler (called when the deferred goes into the 'resolved'
state) watcher.done(function () { console.log('All done.'); })

var slowCounter = function (start, limit) {

    watcher.notify(start, limit);// fire a notify event every time we run

    if(start === limit) {
      watcher.resolve();
      return
    }

    setTimeout(function () {
        slowCounter(start + 1, limit);
    }, 10);

}

# Easing

jQuery.easing is an object that contains the easing functions available to
jQuery. Be default, only:

- linear
- swing

are included. The easing plugin provides a bunch more (and sets the default to
'EaseOutQuad') and jQueryUI includes the easing plugin code. A demo of the
others is available at http://jqueryui.com/demos/effect/easing.html
