# Udacity google performance course

to get 60fps we need to render a new frame every 16ms but browser housekeeping
means we need to complete our work in ~10ms-12ms

How the browser renders a single frame

1. something triggers a visual change on the page e.g. JS or CSS animation
2. "recalculate styles" (tooling name) = combine DOM and CSS trees to make
   render tree
    * render tree is like DOM but only contains _visible_ elements e.g. no
      `<head>` or anything with a style of `display: none;`.
    * NOTE: things that are not in viewport or have 0 height are still in the
      render tree
3. "layout" (tooling name) (also called "reflow")
    * figure out how much space each element in the render tree needs
    * turn the tree into widths, heights, and positions on screen i.e. into
      boxes
    * when layout changes it changes for the whole document i.e. its scope is
      "whole document"
        * any exceptions to this?
        * you can limit the scope of a layout with a "layout boundary"
            * http://wilsonpage.co.uk/introducing-layout-boundaries/
            * seems not super well understood and maybe a micro-optimisation
            * forcing something to be a "layout boundary" means setting an
              explict width, height, overflow on it
4. "paint" (tooling name)
    * turns the vector set of instructions into raster
    * also includes "image decode and resize" which turns JPEG/PNG into bitmap
      for painting on the screen
5. "composite layers"
    * composite the layers together
    * layers are actually painted in square tiles
    * these tiles are created on the CPU and then uploaded to the GPU



The rendering pipeline looks like:

```
JS > Style > Layout > Paint > Composite
```

JS is the "trigger a visual change" thing and could also be a CSS animation or
a web animation API

but the full pipeline is not always used

```
# scenario 1: our "visual change" touches a "layout property"
# => full pipeline used
JS > Style > Layout > Paint > Composite

# scenario 2: our "visual change" touches a "paint only property" e.g.
"background"
JS > Style > Paint > Composite

# scenario 3: our "visual change" only changes the arrangment of layers
JS > Style > Composite
```

Aside: Flexbox is created in CSS but it does not cause "recalculate styles"
when the layout changes, presumably becasue the style has not actually changed,
just the layout on the page

* http://csstriggers.com/ has a good summary of which CSS properties require
  which parts of the pipeline
    * note that transform and translate things only require compositing so are
      the most performant


When perf matters most: RAIL
4 phases of an app from a perf POV

1. Response
    * needs to happen within 100ms to not be noticible
1. Animation
    * can be things that need to stick to the users finger or transitions and
      animations
    * need to provide a new frame every 16ms
1. Idle
     * do work you have defered during the other phases
     * keep this work to 50ms chunks so that the app can still respond to an
       input within 100ms
1. Load
    * ideally happens within 1s


### FLIP: First Last Invert Play

* First = where element starts
* Last = where the element ends up
* Invert =
* Play =

pre-calculate expensive animations

He took the card as layed out on the page and measured teh position of it and
its children with getClientBoundingRect and measured its opacity too

## Lesson 1 summary

* result: https://developer.chrome.com/devsummit/
* code:
  https://github.com/udacity/devsummit/blob/master/src/static/scripts/components/card.js

A summary time budget of the times you have available for RAIL:
http://udacity.github.io/60fps/images/time-table.jpg

A key way of keeping layout and paint times down is to reduce how many elements
they work on.


If making a visual update to the screen from JS use `requestAnaimationFrame` to
make sure the browser runs it at the right time


## requestAnimationFrame

* requestAnimationFrame schedules JS to run at the earliest possible moment in
  each frame
* it is better than `setTimeout` and `setInterval` because they don't align
  their work to frames
    * their timers will expire in the middle of a frame and delay it so it
      doesn't get painted in time

* in pretty much everything except IE9 and older


The JS profiler box should only be ticked when you know you have a JS problem


## Recalculating styles

The cost of "recalculate styles" varies lineraly with the number of elements
There are cases (odd ones, for sure) where is n squared, and sometimes worse

Complex CSS selectors do take more work
"complex" => how much up and down the dom tree travel does the browser have to
do to figure out the CSS


JS properties which reading of will force layout

* offsetHeight
* scrollY
* ... TODO

Be careful of doing things that will trigger layout in loops - rewrite them to
read the "force layout" properties and do all the update changes in a batch

If you trigger a layout thent he browser _has_ to paint which is part of the
reason layouts are so expensive


## The timing API

TODO: do more on this (Wed 13 May 06:40:05 2015)


```js
window.performance.mark("mark_start_frame");

// do stuff ...

window.performance.mark("mark_end_frame");
window.performance.measure("measure_frame_duration", "mark_start_frame",
"mark_end_frame");
window.performance.getEntriesByName("measure_frame_duration");


```

## Paint

Dev tools have a paint profiler which shows a whole timeline of paint actions


Examples of paint actions

* save()
* drawTextBlob()
* clipRect()
* restore()
* drawRect()
* drawBitmapRectToRect()
* setMatrix()


## Layers

http://caniuse.com/#search=will-change

There is a time cost assocated with layer management so be careful when
creating new layers. Your goals are

~2ms in "Update layer tree"
~2ms in "Compositing"

in performance critical animations


```css
/* promote a box to a layer on newer browsers */
/* hint to the browser what to do, not force it */
.foo {
    will-change: transform;
}

/* promote a box to a layer on older browsers */
/* the null transform hack */
.foo {
    transform: translateZ(0);
}
```

A rule: Anytime your JS affects something visilbe on the page, that JS should be wrapped in a requestAnimationFrame


# Critical Rendering Path

1. Fetch HTML
1. Build DOM
    * HTML characters -> Tokens -> Nodes -> DOM
    * Tokenizer
        * emits "start" and "end" tokens for each HTML element so nesting can
          be worked out.

    * The DOM is built incrementally
        * The browser will start processing and rendering it before all the HTML has come down
1. Fetch CSS
1. Build CSSOM
1. Combine DOM and CSSOM to build the "render tree"
    * If a DOM element is not visible on the page it will not be copied into
      the render tree.
    * The render tree is a filtered mash-up of the DOM and CSSOM


The default viewport width for a browser is 980px

    <meta name="viewport" content="width=device-width" />

tells the browser to set its viewport width to the device width

Anytime we update the render tree by

1. modifying styles
2. modifying DOM nodes

there is a good chance we have to re-run layout

1. Layout
1. Paint

If a script is synchronous then it has to wait for the CSSOM to be built before it can be run
    * I guess JS needs to be able to read properties of the DOM Nodes that CSS
      must provide so any CSS files found before a synchronous JS file *must*
      be run first


JS can manipulate both the DOM and the CSSOM!
CSS blocks rendering and also blocks the execution of JS

# Strategies for staying fast

## 1. minify, compress, cache

pretty straighforward

## 2. use media queries to reduce work needed to build CSSOM

You can use any media query in the media attribute of the stylesheet LINK tag -
this informs the browser that it doesn't need to block rendering of the page
until these styles are parsed _if_ the media query doesn't currently apply

    # no media attribute so is blocking by default
    <link rel="stylesheet" href="styles.css" />

    <link rel="stylesheet" href="styles.css" media="screen" />

    # only applies when printing so not render blocking
    <link rel="stylesheet" href="print-styles.css" media="print" />
    <link rel="stylesheet" href="landscape-styles.css" media="screen and (orientation landscape)" />

* -- To make this work all media query styles need to be output grouped together
* -- makes it harder to have MQ that are just for a single piece of content
* ?? can sass be persuaded to build output like this?

## 3. minimize "Parser blocking" JS

Parser blocking JS = Any JS that prevents the browser from completing building
the DOM

There are 2 ways to do this/Attributes for controlling scripts:

* async ("don't block the critical rendering path")
    * tells the browser 2 things:
        1. do not wait for CSSOM to be built
        2. do not block DOM construction
* defer
    * ???

# Critical path metrics

1. How many critical resources do I have?
    * How many resources do I need to download before I can render?
    * Remeber that images don't block that first render
1. How many critical KB must I download before I can render
1. What is the minimum critical path length?
    * How many round-trips must I make to the server before I can render
    * Each round-trip to the server brings back approx 14 KB so ideally keep
      resources on the CRP smaller than that


# Pre-load scanners

some browsers have a "pre-load scanner" that will peek ahead in the HTML tree
and initiaite downloads of critical path resources even while parsing is
blocked (maybe parsing is waiting for CSS or JS)

The answer to the TCP slow start question is 300 mS - this is the wolfram alpha
formula for it

    100 ms * ceiling(log base 2 of (45 / 10))

https://developers.google.com/speed/pagespeed/insights/
