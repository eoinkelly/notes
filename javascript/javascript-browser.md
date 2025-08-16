# Browser Javascript & The DOM

## Inbox & Questions

- How much stack space does each browser give you?

## Including a script in HTML

Script tag attributes

- language="javascript" is decprecated - do not use.
    - it was a result of MS trying to replace JS back in the day
- src="some/url/to/file.js
    - you still need this :-)
- type="text/javascript" is ignored by browser
    - the official type for JS is application/ecmascript" and wasn't finalised
      till 2006 ish so "text/javascript" is wrong
    - older browsers don't recognise the new, correct one so you can't win here
    - the browser will ignore this and just use whatever MIME type the server
      sends the file as anyway
    - The W3C rules say it is required, DC recommends you ignore them.
- This behaviour: <script> <!--

        // -->
        </script>

    isn't necessary unless you need netscape 1 to read the page. It is just
    there to get around a bug in netscape 1.0.

So the correct way to include a script:

<script src="path/to/script.js"></script>

- Script tags should be placed as close to the bottom of the body as possible
    - When the browser sees a script tag it has to be pessimistic about what
      that script might do to the page
    - It has to stop parsing, go fetch the script, then parse it, then resume
      parsing.
    - By putting it at the bottom you let the browser at least have already
      asked for the images etc.
- CSS tags should be placed as high in the head as possible
- Minify and gzip JS files
- Set far future expires headers if possible
- Concatenate them as much as possible unless they are on CDN

- Each <script></script> tag in the browser delivers a "compilation unit" that
  is compiled and immediately executed.
- Since JS does not have a linker it throws them all together in a common global
  namespace
- A compilation unit is a set of executable statements

UNSORTED FOR THIS SECTION

    browsers do speculative parsing
    	while executing srip[t another thread parses the rest of the document looking for external resources that need to be loaded
    	the speculative parser doesn't modify the DOM (the main parser does that) - it just parses references to external resources like scripts, css, images

    firefox will block all scripts when there is a stylesheet still loading as the script may ask for style information and it doesn't want to give wrong answers
    webkit blocks scrpts only when they try to access certain properties that may be effected by unloaded stylesheets
    	I.e. webkit will *stop* your script if you access a property that it knows might be affected by stylesheets that yet to download
    	this is why stylesheets should be loaded as soon as possible

## document.write - do NOT use

- There are better alternatives to it now.
- Called before onload() - it will insert new stuff into the doc
- Called after onload() it will destroy the doc and replace it with new stuff
- ECMAScript want to get rid of it but can't because some old stuff depends on
  it.

# The DOM

the Window object is the main entry point to all client-side JS also known as:
parent, top it represents a web browser window or frame referred to by
identifier 'window' it has properties and methods window.location = a location
object that specifies the current URL window.alert() window.setTimeout()
window.setInterval() window.window // points to the window itself window.top //
points to window window.document = the DOM
window.document.getElementbyId('blah') = returns an 'element' object

## Cross-site scripting

A script can access another window if

1. It can get a reference to it
2. Same origin policy: if document.domain === otherwindow.document.domain you
   can access the other window

Browser detection is brittle because browsers lie - this makes feature detection
a better alternative DC says it is rare to set more than one event handler - he
wnats to know why you can't put them in one?

DC recommends you dev in this order

1. Do what works (be pragmatic)
2. Do what is common. (avoid propreity stuff in browsers)
3. Do what is standard

the window object is the global object in client-side JS it's properties &
methods are effectively global variables and functions

- DOM nodes implement one of the DOM interfaces.
- browsers use concrete implementations that have other attributes used by the
  browser internally. scripts are parsed & executed immediately when the parser
  reaches a script tag _ this means that if the script is external, the parser
  has to wait for it to be downloaded, then execute it before it can continue _
  scripts marked as 'defer' would not halt parsing - they would be executed when
  parsing is complete \* scripts marked 'async' are to be parsed and executed by
  a separate thread

## Collections

document.anchors document.applets document.embeds document.forms

document.all // microsoft only, do not use etc.

DC recommends to avoid them therea re more modern equivalents to all of them -
don't use them

## name vs id

- they are not interchangeable

name identifies values in form data identifies a window/frame

id uniquiely identifies an element so you can access it

## the W3C recommends getting at a node by:

document.getElementById(id) document.getElementsByName(name) // A name can
return more than one element - names are not required to be unique
document.getElementsByTagName(tagName) // A name can return more than one
element- names are not required to be unique

document.getElementsByClassName()
https://developer.mozilla.org/en-US/docs/DOM/document.getElementsByClassName _
returns either a NodeList (a live query on the DOM) or HTMLCollection depending
on the browser _ the methods of NodeList are a subset of those of HTMLCollection
so this isn't a major deal

JS provides direct access to some nodes

document = root node of the document tree document.documentElement = HTML node
(not called HTML as W3C wanted to kill HTML) document.body = BODY node
document.head = HEAD node

- IE and firefox give different DOM trees. W3C requires that the whitespace
  between nodes is captured in the tree too. MS ignores this but Firefox does
  not.
- Inside a host environment, JavaScript can be connected to the objects of its
  environment to provide programmatic control over them. The object of the
  environment provied by a web browser are
    1.  window
    2.  document

DOM Node Pointers Every NODE node has pointers to nodes around it (these
pointers are read-only to web devs)

- firstChild
- lastChild
- nextSibling
- previousSibling
- parentNode
- childNodes // a array of all it's children

// DC walk the DOM code - uses binary tree algorithm to walk the tree function
walkTheDOM(node, func) { func(node); node = node.firstChild; while (node) {
walkTheDOM(node, func); node = node.nextSibling; } }

My faffing around with a visual walk of the DOM function walkTheDOM(node, func)
{ setTimeout(3000, func(node)); node = node.firstChild; while (node) {
walkTheDOM(node, func); node = node.nextSibling; } }

    walkTheDOM(document.body, function (n) {
    	if (n.style) {
    		n.style.backgroundColor = "#ff00ff";
    	}
    });

## Getting at nodes in the DOM

node = document.getElementById(id) // returns a node

nodeList = document.getElementsByTagName(name) // returns a nodeList object (not
an array of nodes) nodeList = document.getElementsByName(name) // returns a
nodeList object (not an array of nodes)

- nodelists are live queries on the DOME an can be ineffecient

## Manipulating a node

Once you get hold of a node you can manipulate it by

// old school way node.property = expression node.src = 'http://blah.com';

node.getAttribute('src'); // W3C way node.setAttribute('src',
'http://www.example.com/img.png'); // W3C way // DC likes that since set has
side-effects he likes that it is an invocation

// Change class name var classes = node.className; node.className = "mod
mod-hello";

Getting the value of a CSS property on a node: node.style.<stylename> = <value>;
// appears to work in new chrome at least, not sure what else???

Finding out what the current value of a particular style
node.currentStyle.stylename // IE only
document.defaultView().getComputedStyle(node, "").getPropertyValue(<stylename>);
// The Java inspired W3C way

## Creating New Nodes

document.createElement(tagName) // name can be uppercase or lowercase
document.createTextNode(text) node.cloneNode() // clone an individual element
node.cloneNode(true) // clone a node and all it's descendants

- The new nodes created with any of the above methods are not automatically
  connected to the document

To add new nodes to the tree do node.appendChild(new) node.insertBefore(new,
sibling) // place new before sibling node.replaceChild(new, old) // node needs
to the parent of old, new

    // so most of the time you'll do something like this
    old.parentNode.replaceChild(new, old); // This is arkward (specifying old twice)
    old.parentNode.removeChild(old) // This is a arkward way of expressing this but we have to use it
    // In old IE, you have to manually removed event handlers or you get memory leaks

// The W3C does not give access to the HTML parser but Microsoft did
node.innerHTML // invented by Microsoft, not in any official standard, gives
access to the HTML parser, is a security hazard // all browsers implement
.innerHTML

So you have 2 choices when adding stuff to the document

1. create your nodes and insert/append them to the tree
2. Build up your new stuff in text and use .innerHTML to parse it into the tree
   (this is faster accroding to DC)

- When choosing here don't worry so much about performance - optimise for
  whatever makes code cleaner and easier to understand/maintain

## Browser & Security (Crockford on JS 5)

- strict mod makes i possible to have static verificaiton of third party code
  without over-constraining the programming model
- ES5/Strict can protect a page from it's widgets but i cannot protect the
  widgets from the page
- ES5/Strict cannot protect the page from XSS
- DC believes that template based web frameworks (ASP, PHP etc.) leads you down
  the wrong path security wise

Many approaches to security fail:

- Security by obscurity (try to make it so complicated that it will slow an
  attacker)
- Security by identity (if we know something about who wrote the code it tells
  us something about it)
- Security by vigilance (too much info to handle - current state of the art)

## JS is "Pass reference by value"

- JS is a pass by reference language - when you pass a value to a function, you
- are not passing a copy of the value or passing the name of it, you are passing
  a reference (this would be a pointer in older languages)

    x = "foo"; a = x; b = x; a === b; // returns true // === only returns true
    if a and be are the _same_ object, it will not be true if they are different
    objects that contain the same thing

- When values are immuatable pass by value and pass by reference can look the
  same
- Don't confuse values with variables
- Consider:

    function funky(o) { o = null; }

    var x = "hello"; // we are passing funky the value that x refers to. funky()
    knows nothing about x funky(x); // x is still "hello"

funky doesn't know anything about x - it only knows about the value that x
refers to

If you think of JS variables as holding the objects themselves the you can
consider JS pass by reference If you think of JS variables as holding references
to the objects then you can consider JS pass by reference

The thing that is passed-in is passed by value but that thing is a refernce this
may be called "reference by value"

A side-effect of this is if you pass an object to a function, then any
modificaiton the function makes to the internals of the object _will_ persist.

function funky(x) { x.name = "Eoin"; }

var y = { name: "John" }; funky(y); // Give funky a reference to the object that
y also points at console.log(y); // => "Eoin"

function do_it(inputs, callback) { // I just calculate my result and give it to
callback(). I don't know what callback() does, only that it is my "delivery man"
callback(result); }

- callback() cannot get access to any do_it() internal state
- do_it() cannot access any callback() internals either

# Security

Principles

- Deterrence is not effective (you can't punish a bot)
- Kerchoffs Principle: The design of a system should not require secrecy; and
  the compromise of a system should not inconvenience the correspondants.
- There is no security in obscurity. The more security you have, the harder they
  are to keep
- Cryptography is not security.
- Security must be factored into every decision
- You can't add security just like you can't add reliability. You can only
  remove sources of insecurity and unreliability
- The impossible is not possible. If a measure is not effective, it is
  ineffective.
    - You shouldn't be trying to do things that are't affective
    - "Well if we can't stop them then we can slot them down with speedbumps"
      attitude is wrong as you are wasting time making speed bumps with time you
      could spend on doing something effective.
- False security is worse than no security - unnecessary expense at the
  confusion of risk
- The principle of least authority: Any piece of software shouldb e given just
  hte capabilities require dto do it's job and _no_ more.
    - Came out of the actor model
- Confusion aids the enemy

The Actor Model

- An actor is a computational entity
- An actor can only send messages to other actors if it knows their addresses
- An Actors can create new actors
- An actor can receive messages

- Web workers are actors
- Web services are not
    - Waterken applies the actor model to web services

- THe address of an actor is a capatility
- A reference to an object is a capability

A reference cannot be obtained by just knowing the name of a global variable or
a class

4 Possible Weaknesses

1.      Arrogation
        * Arrogation = To take or claim for oneself without right
        * Examples: global variables (javascript), public static variables (java)
        * Address generation
        * Known URLS
        * Standard Libraries that provide powerful capabilities (access to filesystem, network, OS) to all programs
2.  Corruption \* It should not be possible to tamper with the system or other
    objects
3.  Confusion It should be possible to create objects that are not subject to
    confusion
4.  Collusion It should not be possible for 2 objects to communicate until theya
    re formally introduced

Every obbject should be given the capabilities it needs to do it's work and no
more. Good designs have information hiding and also capability hiding Objects
shold be introduced on a "need to do" basis

Facet objects are lightweithght intermediate object

- References are not revokable. Once you introduce an object you cannot depend
  on the object obeying the request to revoke

DC believes that web stack complexity is a cause of insecurity - too many
languages each with their own methods of

1. encoding
2. escaping
3. quoting
4. commenting

Examples of languages:

- HTML
- URLs (hadn't thought of this!)
- CSS
- Cookie language
- Javascript

ECMA committee is trying to turn Javascript into an "Object Capability Language"
which will improve security.

# Object Capability Security

- A reference is a capability
- There should be exactly 3 ways to get a reference to an object and by no other
  means
- More at http://www.youtube.com/watch?v=eL5o4PFuxTY

1.  By Creation If a function creates an object it gets a reference to that
    object (pretty obvious)

2.  By Construction An object may be endowed by it's constructor with references
    (e.g. things it gets as part of it's closure) This can include references in
    the constructors context and inherited references

3.  By Introduction

        A has references to B and C
        B has no references so it cannot communicate with A or C
        C has no references so it cannot communicate with A or B

        A decides that it would be useful for B to be able to communicate with C
        A calls B, passing a reference to C as a parameter (does an introduction)
        B can now communicate with C, it has hte *capability* to communicate with C
        In the above example:
        	B = do_it()
        	C = callback()

A capabililty is just a reference to an object or function which allows you to
maniuplate or call that function.

If refernces can be created only by creation, construction or introduction then
you _may_ have a safe system. If references can be obtained in any other way,
you do not have a safe system. JS is not yet a capability language because of
the global object. If you know the name of a global variable it gives you the
capability to interact with that global variable - this breaks object capability
security.

# Performance & Optimisation

Hoare's dictum: Premature optimization is the root of all evil.

- Find where the time is being spent. It is counter-productive to speed up
  things where time is not spent (note it's an active negative as you may be
  degrading the quality of the code)
- Don't optimise without measuring

How fast do we want stuff to be?

## Big O notation

- http://rob-bell.net/2009/06/a-beginners-guide-to-big-o-notation/
- Big O notation is used in Computer Science to describe the performance or
  complexity of an algorithm.
- Big O specifically describes the worst-case scenario, and can be used to
  describe the execution time required or the space used (e.g. in memory or on
  disk) by an algorithm.

O(1) describes an algorithm that will always execute in the same time (or space)
regardless of the size of the input data set. O(N) describes an algorithm whose
performance will grow linearly and in direct proportion to the size of the input
data set. Example: a for loop that interates though an array but doesn't do
anything with each element O(N^2) represents an algorithm whose performance is
directly proportional to the square of the size of the input data set. This is
common with algorithms that involve nested iterations over the data set. Deeper
nested iterations will result in O(N3), O(N4) etc. O(2^N) denotes an algorithm
whose growth will double with each additional element in the input data set. The
execution time of an O(2N) function will quickly become very large.

O(log N) e.g. an input data set containing 10 items takes one second to
complete, a data set containing 100 items takes two seconds, and a data set
containing 1000 items will take three seconds. Doubling the size of the input
data set has little effect on its growth as after a single iteration of the
algorithm the data set will be halved and therefore on a par with an input data
set half the size. In mathematica: plot Log(10,n) from 0 to 10000

    total-time = cost-per-iteration * number-of-iterations
    t = c * n

- Sometimes optimising c helps, but not if n is very large
- The n component often overwhelms the c component to the point that you can
  ignore the c component

O(1) = constant O(log n) = Logarithmic O(n) = Linear O(n log n) = Loglinear time
In mathematica: plot (n \* (Log(10,n)) from 0 to 10000 This takes O(n^2) =
Exponential O(n!) = Factorial

As you move down the table above, c becomes increasingly irrelevant

Real world human Performance Thresholds

- Distraction ~ 100ms
- Frustration ~ 1 Sec
- Session Failure ~ 10 Sec
- Impractical
- Infeasible
- Impossible (some crypto algorithms have this as a goal)

To get under the distraction limit it is necessary to give immediate feedback

- JS Arrays are slow Finding an element in an array:
    - In some languages this is O(1)
    - Can be O(log n) or O(n) in JS (worse in IE-8 which uses linked-list not
      hash-tables for array)
    - A loop that should be O(n) can be O(n^2)
    - Don't tune for quirks - don't do short-term optimisations on how
      javascript egines work - they can/will change and there are big variations
      between them.
    - Managing the _triggering_ of layout and rendering speed is probably a lot
      more important than javascript execution speed
    - Sometimes the "Shiny Shit"(TM) can be the main source of slowness

If you are measuring performance there can be variations due to OS clocks etc.
you need to repeat your timing tests to be confident about the numbers.

DC really likes Google Speed Tracer tool

    window, document, element objects all have event-handler properties
    	these properties contain functions (which are first class variables in JS)
    	JS controls the behaviour of a page through event handlers
    	examples
    		window.onload = triggered when the content of the document displayed in the window is stable and ready to manipulate

the browser generates and event whenever something interesting happens to the
document or browser or to some element or object associated with it. "event
type" a string that specifies what "type of event occurred - the event-type is
also called 'event name'e.g. mousemove keydown load "event target" is the object
on which the event occurred or with which the event is associated

NB events have both a type and a target! when referring to an event, you should
mention both e.g. a load event on a window a click event on a button an event
handler is a function that responds to an event applications register their
event handler functions with the browser specifying both an event type and
target An 'Event Object' is an object associated with a particular event and
contains details about it event objects are passed as arguments to the event
handler function all event objects have a 'type' and a 'target' property as well
as properties specific to that event events bubble up e.g. a click on a link in
a div will bubble from link -> div -> body some events have default actions
associated with them .e.g. clicking on a hyperlink follows the link and loads a
new page event handlers can prevent this

One of te first events to occur is the 'load' event - indicates that the
document is fully loaded and ready to be maniuplated. JS programs often use this
event as a trigger or start signal

onload signals the start of phase 2 (ish)

Core & client-side JS have a single-threaded execution model scripts & event
handlers are executed one at a time without concurrency! the browser will never
run 2 event handlers at the same time this means that JS scripts & event
handlers must not run for too long as the user will notice the delay HTML5 web
workers do provide a background thread for computationally intensive tasks (so
that UI does not freeze

defer and async are boolean attributes defined in HTML 5 that cause scripts to
be executed differently the standard says they are only valid for external JS
files

<script defer src="foo.js"></script>
<script async src="foo.js"></script>

TODO read up more on these

Can force script to load asynchronously by dynamically creating a
<script></script> element and inserting it into the document

Event handlers are "registered" in phase 1 so that they will be called when the
event happens in phase 2 I am not invoking the function - I am just registering
the function so the browser will invoke it at the right time the browser invokes
JS functions when it chooses to!

window.addEventListener() doesn't clobber other handlers on the same event

the timer event handlers are registered differently than normal event handlers -
they are usually called 'callbacks' instead of "handlers" but they are
asynchronous too

the nice thing about jquery is that it defines a new cleint-side API and
implements it consistently cross-browser

A cardinal rule of JavaScript accessibility is to design your code so that the
web page on which it is used will still function (at least in some form) with
the JavaScript interpreter turned off. try to favor device independent evnts
like onfocus, onchange rather than onmouseover, onmousedown

-

## Render Tree

there isn't necessiarly a 1:1 relationship between the DOM (content tree) and
the render tree non visual elements won't be in render tree e.g. head DOM
attributes with display attribute set to 'none' won't be in the render tree A
single DOM element can correspond to multiple visual elements e.g. select box
(creates a display area, dropdown list, button in the render tree) text that is
broken up into multiple lines broken HTML. inline elments should only contain
either block elements OR inline elements - if they contain both extra DOM
elements must be created. **well formed HTML is important** floats and
absolutely positioned objects correspond to DOM elements but there are in
different places in the trees

## Embedding JS in HTML

There are 4 ways to embed JS in HTML

1. inline between <script></script> tags
2. from external file with <script src=""></script> behaves exactly as if the
   script had been pasted in at the same spot - the origin of the script does
   not matter
3. In an HTML event handler attribute
   <a href="" onclick="..." onmouseover="..."></a>
4. In a URL that uses the special javascript: protocol it is treated as a single
   line of code (don't use // comments) the content returned by the URL is the
   return value of the code - if you return undefined, there is no content used
   in bookmarklets some browsers will allow thse URLs to change the document,
   some do now Example
   <a href="javascript:void window.open('about:blank');">Open Window</a> the
   void stops the return value of window.open being used to overwrite the
   document

3, 4 are not so common any more 2 gives the cleanest separation between content
and logic so is probably best

There is no formal defn of a "program in client side JS A JS "program consists
of _ all the JS cod in a web page (inline scripts, HTML event handlers,
javascript: URLs) _ and all the external JS referenced with
<script src=""></script> ALl those bits of code share a single global Window
object they all see the same DOM they all have the same global functions and
variables

JS code in an iframe has a different global object that the code in the
embedding document it can be considered a separate javascript program however if
they are both from the same server, they _can_ interact

bookmarklets exist outside of any document can think of them as user extension
when run the bookmarklet gets access to the global object & content of the
current document

# JS Program execution phases

JS Program execution happens in 2 phases

1. document content is loaded and <script> (both inline and external) are run.
   scripts generally (but not always) run in the order in which they appear in
   the document
2. Phase 2 is asynchronous & event driven web browser invokes event handler
   functions defined by HTML event handler attributes scripts executed in the
   first phase previously invoked event handlers in response to events that can
   occur asynchronously

## Idealised Browser Timeline

1. browser creates the DOM and begins parsing the page (document.readyState =
   'loading' during this)
2. when the parser sees a <script></script> tag that do not have defer or async,
   it pauses parsing and executes the inline or external script the parser
   doesn't know if these scripts will use document.write so it has to pause
   these scripts can see their own script element and the document content that
   comes before it
3. when the parser encounters a script with async set it begins downloading the
   script and continues parsing. the script is executed as soon as possible
   after being downloaded but it does not moake the parser stop & wait. async
   scripts must not use document.write() they can see their own script element
   and the document content that comes before it
4. when the document is completely parsed the browser sets document.readyState =
   'interactive'
5. Any scripts with the defer attribute are executed in the order in which they
   appeared in the document async scripts _may_ also be executed at this time
   defered scripts have access to the whole document tree and must not use
   document.write()
6. The browser fires a DOMContentLoaded event on the document object
    - this marks the transition from synchronous to asynchronous script
      execution
    - there may still be async scripts that have not yet executed at this time.
    - jquery triggers .ready()
    - DOMContentLoaded is:
        - fired when the page has been parsed but images and stylesheets are not
          necessairly loaded
        - blue line in chrome dev tools
        - DOMContentLoaded isn't supported in IE 6-8
7. the document is completely parsed now but the browser may still be waiting
   for images etc. to load when all such content finishes loading and when all
   async scripts have loaded & executed, the document.readState = 'complete' and
   the browser fires a 'load' event on the Window object. jquery triggers
   .load() attach to it by $('window').load(function(){}) onload is: fired when
   all files have finished downloading from all resources incl. images,
   stylesheets red line in chrome dev tools
8. From then on, event handlers are invoked asynchronously in response to input
   events timer expirations network events

More info

- https://developer.mozilla.org/en/DOM/DOM_event_reference/DOMContentLoaded
- http://www.whatwg.org/specs/web-apps/current-work/multipage/the-end.html

document.readyState propertiese differ from browser to bowser

async support is not common http://caniuse.com/script-async not in IE6-9 or
safari pre 5 shitty mobile browser support

defer is supported by all versions of IE but only now being implemented by
others http://caniuse.com/script-defer has good browser support, not in safari
pre-5 or opera at all shitty mobile browser support

To read:
http://www.aaronpeters.nl/blog/why-loading-third-party-scripts-async-is-not-good-enough
quotes script-inserted scripts that are inserted into the DOM by appendChild or
insertBefore do delay window.onload. onload is accused of being a vanity metric
I think it does impact UX, as the activity indicator within IE disappears at the
onload event. So then, it increases 'perceived performance'.

            Several websites rely on "DOMContentLoaded" and "onload" to trigger functionality - anything rom slideshow starting to content appearing. In these cases bad third party performance could delay the triggering of the event in the browser, hence breaking functionality. In several cases the impact is also displayed via the mouse pointer, which can signal the user the page is not done - hence giving the impression something is missing and the page is not complete (which is bad).
    http://www.phpied.com/social-button-bffs/
    http://stackoverflow.com/questions/7718935/load-scripts-asynchronously

# Loops in JS

// While loop as recursion

(function whiler() { if (<condition>) { <body> return whiler(); } }());

- JS does not implement tail recursion optimisation or "proper tail calls" -
  this lack will cause the program to slow down as it accumulates memory and
  eventually fail so don't use the whiler() pattern above.

# Event Loops

- Event queue containing callback functions (timer(setTimeout(), setInterval(),
  ui (clicks etc.), network(script loaded, loading complete))
- Turn: remove one callback from the queue. it runs to completion
- Prime directive: Never block, never wait. Finish fast.

- Avoid: alert(), confirm() prompt(), XHR in Synchronous Mode (XHR also has an
  asynch mode) because all of these block the browser thread

JS does not have synchronous read/write - this is actually a good thing.

- Event loop is only one approach to concurrency
-

Threading Pros _ no rethinking required _ Blocking programs are ok _ Execution
continues as long as any thread is not blocked Threading Cons _ Stack memory per
theread (not a big deal anymore) * If two threads are using the same memory a
race *may* occur (which is worse than a race *will* occur) * Overhead _ Deadlock
_ Thinking about reliability is extremely difficult \* System/Application
confusion. DC thinks java couldn't decide between being a systems language or an
application language.

Event Loop Pros

- Completely free of races and deadlock
- Only one stack
- Very low overhead
- Resilient. If a turn fails the program can still go on.

Even Loop Cons

- Programs must never block
- Programs are inside out
- Turns must finish quickly (event loops are not suitable for all programs esp.
  ones that take a long time to finish a task)

2 solutions to Long running tasks in event loop model

- Eteration - break the task into multiple turns
- Move the task into a separate process (workers).

RPC

- Combiness 2 great ideas: functions and netwroking to produce a really bad idea
- Liek READ attempts to isolate programs from time. The program blacks out.
- In reading the program it is by design difficult to see where time is lot
  \*This can result in a terrible experience for the user.

Writing secure apps using threads is very difficult as it's hard to have
consistency

XSS caused by

1. sharing of the global object
2. Misinterpretation of HTML. Has many languages that have diff encoding patters
   and can be nested inside each other in various ways

Page Templates (PHP, ASP, JSP) are too rigid a framewaork and it's too easy to
insert text into the context that gets delivered as the HTML load

node.js is a high performance event pump

DC thinks that HTTP is dumb and we are stuck with it because it's the only thing
many firewalls and proxies will pass

At the end of every turn the stack gets reset Strategies for solving the race
condition

- semaphonre
- monitor
- rendezvous
- synchronization

Only one thread can be executing on a critical section at a time

- All other threads wanting to executue the critical section are blocked
- If threads on't interact then th eprogram ruuns at rfull speed
- If they do interact, then races will occur unless mutual exclusion is
  employed - this can cause deadlock

Deadlock

- Occurs when threads are waiting on each other
- Races and deadlocks are difficult to reason about.
- They are the most difficult problems to identify, debug and correct

Managing sequential logic is hard. Managing multi-dimensional temporal logic is
even harder.

# Events

- The browser has an event-driven, single-threaded, asynchronous programming
  model.
- Events are targetted at a particular node
- Events cause the invocation of event handler functions

Events are triggered by timers network activity user input (mouse, keyboard
etc.)

## 3 Event Handler APIs (3 ways of adding event handlers)

Netscape node["on" + type] = handlerFunc; Microsoft node.attachEvent("on" +
type, handlerFunc); W3C node.addEventListner(type, handlerFunc, false); // The
3rd parameter is _required_ and should be false according to DC

- Event handlers take an optional event object
- Microsoft do not pass in an event object to event handlers - they want you to
  find the global 'event' object
- Modern browsers do event bubbling. The event starts at the target node - if it
  doesn't handle it then the event is hander to it's parent and so on up the
  tree.

function (e) { // Example event handler // event is a global object that
microsoft sets up for the JS environment e = e || event;

    // Diff browsers have a diff opinion about what the name of the interesting thing is
    var target = e.target || e.srcElement; // the target node has diff names in diff flavors of events

    // do useful stuff

    // This code block cancels bubbling in a x-browser way
    e.cancelBubble = true;
    if (e.stopPropagation) {
    	e.stopPropagation();
    }

    // This codes prevents the browser from executing the default actions for this event (e.g. click)
    e.returnValue = false;
    if (e.preventDefault) {
    	e.preventDefault();
    }
    return false;

}

IE6 uses refernce counting garbage collecting but this doesn't work if you have
cycles (two objects with references to each other, ref count will never go to
0). This doesn't happen in the DOM but does happen with events. This means you
have to explicitly remove all the events attached to a node before deleting the
node.

- Touching a node has a cost
- Reflow can have a big cost
- Repaint can have a big cost
- Styling can have a big cost
- Random things like nodelist an have a big cost
- Usually Javascript has a small cost

Most of your programs time is spent in the DOM, not executing your code!

Sometimes in a memory managed system it's possible to hang on to too much state
preventing it from being garbage collected

## Console API

- Implemented by the browsers - not in old IE
- console.log('hellol'); // Print result to console

## Debugging

This statement will cause execution to pause in most javascript implementations
(according to crockford) debugger;

# New stuff in ES5

ES5 has standards for two languages: default, strict

This stuff won't work on IE < 9

- Trailing commas are OK (you can think of them as delimiters rather than
  separators)
- Reserved word relaxation var a = { class: "foo", function: "blah", }
- Getters and setters (downside: getting and setting can now have side-effects!)

- multi-line string literals (DC doesn't like them)
- The following are now constant: Infinity, NaN, undefined
- Fixed parseInt - no longer requires radix parameter
- Regexp literals now produce a new regular expression objects each time they
  are evaluaged
- Replacing Object, Array does not change behaviour of {}, [] now
- Added lots of new methods (because they can be added without breaking syntax
  and can be polyfilled)
    - JSON built-in (uses json2.js interface)
- String.prototype.trim
- Function.prototype.bind (allows you to bind a particular this - "lets you use
  a method in places where an ordinary function is expected ??? don't understand
  this")
- Array.prototype.every
- Array.prototype.filter
- Array.prototype.forEach (DC recomends this - speed diff is not significant
  according to him)
- Array.prototype.map
- Array.prototype.reduceRight
- Array.prototype.reduce
- Array.prototype.some
- Date.now()
- Object.keys
- Object.create (creates a new object using an old object as a prototype) Can be
  simulated in ES3 except that the new one can use null as prototype and old
  cannot

# Programming Style and your Brain

- No space before ( when used to invoke a function
- No space between a function name and a parameter list
- Once space between all othe rnames and (
- Distinguish between parens for invocation and parens for grouping

Correct: foo(blah) return (a+b); if (a===0) {...} function foo(b) {} function
(x) {...}

# IIFE

- This doesn't work because if JS sees function in the frist thing then it
  assumes it's a function statement.

function () { // do stuff ... }();

- To solve this we wrap it in parens

(function () { // do stuff ... }());

- Declare all of your variables at the top of the function
- Delcalre all your functions before you call them (JS hoisting will let you not
  do this but it is confusing)
- The initial caps convention lets you tell whether a function is designed to be
  used with new - this is important as if you use a constructor without new then
  you will clobber global variables

// Consider this: var a = b = 0;

// It appears to do: var a = 0; var b = 0;

// but it really does this: var b = 0; var a = b;

DC doesn't like ++ (he prefers x += 1) he finds himself trying to put too much
stuff on one line

x += 1 // same as ++x

how are x++ and ++x different?

Programming is the most complicated thing that humans do Designing a programming
style should choose features because we want to improve our error rate

# DC - DOM - Inconvenient API notes
